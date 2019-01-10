#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <memory>

#include <Array.h>
#include <Map.h>
#include <veri_file.h>
#include <VeriId.h>
#include <VeriExpression.h>
#include <VeriConstVal.h>
#include <VeriMisc.h>
#include <VeriModule.h>
#include <VeriStatement.h>

#include <tinycbor/cbor.h>

using namespace Verific;

template<typename T>
struct IdMap {
    std::map<const T*, uint32_t> obj_ids;
    std::set<const T*> seen;

    IdMap() : obj_ids() {
        obj_ids.emplace(nullptr, 0);
    }

    uint32_t map(const T* ptr) {
        auto iter = obj_ids.find(ptr);
        if (iter != obj_ids.end()) {
            return iter->second;
        } else {
            uint32_t id = obj_ids.size();
            obj_ids.insert(std::make_pair(ptr, id));
            return id;
        }
    }

    // Map ptr to an ID, and also mark it as seen.  Use this when getting the
    // ID of the node currently being encoded.
    uint32_t map_seen(const T* ptr) {
        uint32_t id = map(ptr);
        seen.insert(ptr);
        return id;
    }
};

void cbor_check(CborError err) {
    if (err == CborNoError || err == CborErrorOutOfMemory) {
        return;
    }
    std::cerr << "encoding error: " << err << "\n";
    abort();
}

struct SubEncoder;

struct Encoder {
    std::unique_ptr<CborEncoder> ce;

    // NB: `ce` must be manually initialized (`cbor_encoder_init`) before use.
    Encoder() : ce(new CborEncoder) {}
    Encoder(const Encoder&) = delete;
    Encoder(Encoder&& other) : ce(std::move(other.ce)) {}
    Encoder& operator=(const Encoder&) = delete;

    SubEncoder array(size_t len=CborIndefiniteLength);

    void uint(uint64_t value) {
        cbor_check(cbor_encode_uint(ce.get(), value));
    }

    void negative_int(uint64_t value) {
        cbor_check(cbor_encode_negative_int(ce.get(), value));
    }

    void int_(int64_t value) {
        cbor_check(cbor_encode_int(ce.get(), value));
    }

    void string(const std::string& value) {
        cbor_check(cbor_encode_byte_string(ce.get(), (const uint8_t*)value.data(), value.size()));
    }

    void null() {
        cbor_check(cbor_encode_null(ce.get()));
    }

    void bool_(bool value) {
        cbor_check(cbor_encode_boolean(ce.get(), value));
    }
};

struct SubEncoder : public Encoder {
    CborEncoder* parent;
    SubEncoder(CborEncoder* parent) : Encoder(), parent(parent) {}
    SubEncoder(const SubEncoder&) = delete;
    SubEncoder(SubEncoder&& other) = default;
    SubEncoder& operator=(const SubEncoder&) = delete;
    ~SubEncoder() {
        if (ce) {
            cbor_check(cbor_encoder_close_container(parent, ce.get()));
        }
    }
};

SubEncoder Encoder::array(size_t len) {
    SubEncoder subenc(ce.get());
    cbor_check(cbor_encoder_create_array(ce.get(), subenc.ce.get(), len));
    return subenc;
}

/*
struct AstEncoder {
    IdMap<VeriTreeNode> ids;
    Encoder enc;

    void encode(VeriTreeNode* n) {
    }
}
*/

template<typename T, typename U>
T* exact_cast(U* ptr) {
    if (typeid(*ptr) == typeid(T)) {
        return (T*)ptr;
    } else {
        return nullptr;
    }
}

bool array_empty(Array* a) {
    return a == nullptr || a->Size() == 0;
}

void encode_tree_nodes(IdMap<VeriTreeNode>& ids, Encoder& enc, Array* a);

void encode_tree_node(IdMap<VeriTreeNode>& ids, Encoder& enc, VeriTreeNode* x) {
    if (x == nullptr) {
        enc.null();
        return;
    }

    uint32_t x_id = ids.map_seen(x);

    SubEncoder subenc(enc.array());
    subenc.uint(x_id);
    subenc.string(typeid(*x).name());

    if (auto m = exact_cast<VeriModule>(x)) {
        subenc.string(m->Name());
        encode_tree_node(ids, subenc, m->GetId());
        //encode_tree_nodes(ids, subenc, m->GetPorts());
        //encode_tree_nodes(ids, subenc, m->GetParameters());
        encode_tree_nodes(ids, subenc, m->GetItems());
        //encode_tree_nodes(ids, subenc, m->GetPortConnects());
        //encode_tree_nodes(ids, subenc, m->GetParameterConnects());
        //encode_tree_nodes(ids, subenc, m->GetPackageImportDecls());
    } else if (auto mi = dynamic_cast<VeriModuleInstantiation*>(x)) {
        subenc.uint(ids.map(mi->GetInstantiatedModule()));
        //encode_tree_nodes(ids, subenc, mi->GetParamValues());
        encode_tree_nodes(ids, subenc, mi->GetInstances());
    } else if (auto ii = dynamic_cast<VeriInstId*>(x)) {
        subenc.string(ii->Name());
        encode_tree_nodes(ids, subenc, ii->GetPortConnects());
    } else {
        std::cerr << "unsupported AST node: " << typeid(*x).name() << "\n";
    }
}

void encode_tree_nodes(IdMap<VeriTreeNode>& ids, Encoder& enc, Array* a) {
    if (a == nullptr) {
        enc.array(0);
        return;
    }

    SubEncoder subenc(enc.array(a->Size()));
    size_t i;
    VeriTreeNode* x;
    FOREACH_ARRAY_ITEM(a, i, x) {
        encode_tree_node(ids, subenc, x);
    }
}

void encode_project(IdMap<VeriTreeNode>& ids, Encoder& enc) {
    Map* modules = veri_file::AllModules();

    SubEncoder subenc(enc.array());

    MapIter map_iter;
    const char* name = nullptr;
    VeriModule* module = nullptr;

    FOREACH_MAP_ITEM(modules, map_iter, &name, &module) {
        encode_tree_node(ids, subenc, module);
    }
}

// Scan the entire AST, 

/*
void scan_project(Ids& ids);
void scan_module_item(Ids& ids, VeriModuleItem* x);
void scan_module_items(Ids& ids, Array* a);
void scan_expression(Ids& ids, VeriExpression* x);
void scan_expressions(Ids& ids, Array* a);
void scan_id_def(Ids& ids, VeriIdDef* x);
void scan_id_defs(Ids& ids, Array* a);
void scan_misc(Ids& ids, VeriTreeNode* x);
void scan_miscs(Ids& ids, Array* a);

void scan_project(Ids& ids) {
    MapIter map_iter;
    VeriModule* module = nullptr;
    FOREACH_VERILOG_MODULE(map_iter, module) {
        scan_module_item(ids, module);
    }
}

void scan_module_item(Ids& ids, VeriModuleItem* x) {
    if (!ids.module_items.record(x)) {
        // Recurse into this module only the first time we see it, to avoid
        // duplicating work.  
        return;
    }

    if (auto m = dynamic_cast<VeriModule*>(x)) {
        scan_id_defs(ids, m->GetPorts());
        scan_id_defs(ids, m->GetParameters());
        scan_module_items(ids, m->GetParameterConnects());
        scan_expressions(ids, m->GetPortConnects());
        scan_module_items(ids, m->GetModuleItems());
        scan_module_items(ids, m->GetPackageImportDecls());
    } else if (auto mi = dynamic_cast<VeriModuleInstantiation*>(x)) {
        scan_expressions(ids, mi->GetParamValues());
        scan_id_defs(ids, mi->GetInstances());
    } else if (auto dd = dynamic_cast<VeriDataDecl*>(x)) {
        scan_id_defs(ids, dd->GetIds());
    } else if (auto ca = dynamic_cast<VeriContinuousAssign*>(x)) {
        scan_expressions(ids, ca->GetDelay());
        scan_miscs(ids, ca->GetNetAssigns());
    } else if (auto ac = dynamic_cast<VeriAlwaysConstruct*>(x)) {
        scan_module_item(ids, ac->GetStmt());
    } else {
        std::cerr << "unsupported ModuleItem: " << typeid(*x).name() << "\n";
    }
}

void scan_module_items(Ids& ids, Array* a) {
    size_t i;
    VeriModuleItem* x;
    FOREACH_ARRAY_ITEM(a, i, x) {
        scan_module_item(ids, x);
    }
}

void scan_expression(Ids& ids, VeriExpression* x) {
    if (!ids.expressions.record(x)) {
        return;
    }

    if (auto apd = dynamic_cast<VeriAnsiPortDecl*>(x)) {
        scan_id_defs(ids, apd->GetIds());
    } else if (auto ir = dynamic_cast<VeriIdRef*>(x)) {
        scan_id_def(ids, ir->GetId());
    } else if (auto bo = dynamic_cast<VeriBinaryOperator*>(x)) {
        scan_expression(ids, bo->GetLeft());
        scan_expression(ids, bo->GetRight());
    } else if (auto ii = dynamic_cast<VeriIndexedId*>(x)) {
        scan_id_def(ids, ii->GetId());
        scan_expression(ids, ii->GetIndexExpr());
    } else if (auto iv = dynamic_cast<VeriIntVal*>(x)) {
    } else if (auto iv = dynamic_cast<VeriConstVal*>(x)) {
    } else if (auto pc = dynamic_cast<VeriPortConnect*>(x)) {
        scan_expression(ids, pc->GetConnection());
    } else if (auto pc = dynamic_cast<VeriUnaryOperator*>(x)) {
        scan_expression(ids, pc->GetArg());
    } else if (auto r = dynamic_cast<VeriRange*>(x)) {
        scan_expression(ids, r->GetLeft());
        scan_expression(ids, r->GetRight());
        scan_expression(ids, r->GetNext());
    } else if (auto r = dynamic_cast<VeriConcat*>(x)) {
        scan_expression(ids, r->GetCycleDelayRange());
    } else {
        std::cerr << "unsupported Expression: " << typeid(*x).name() << "\n";
    }
}

void scan_expressions(Ids& ids, Array* a) {
    size_t i;
    VeriExpression* x;
    FOREACH_ARRAY_ITEM(a, i, x) {
        scan_expression(ids, x);
    }
}

void scan_id_def(Ids& ids, VeriIdDef* x) {
    if (!ids.id_defs.record(x)) {
        return;
    }

    if (auto v = dynamic_cast<VeriVariable*>(x)) {
        // It's okay if `GetInitialValue` returns null: the `IdMap` constructor
        // inserts a mapping from `nullptr` to `0`, so null is always
        // considered "already seen".
        scan_expression(ids, v->GetInitialValue());
        scan_expression(ids, v->GetDimensions());
    } else if (auto ii = dynamic_cast<VeriInstId*>(x)) {
        scan_expressions(ids, ii->GetPortConnects());
    } else if (auto pi = dynamic_cast<VeriParamId*>(x)) {
        scan_expression(ids, pi->GetInitialValue());
        scan_expression(ids, pi->GetDimensions());
    } else {
        std::cerr << "unsupported IdDef: " << typeid(*x).name() << "\n";
    }
}

void scan_id_defs(Ids& ids, Array* a) {
    size_t i;
    VeriIdDef* x;
    FOREACH_ARRAY_ITEM(a, i, x) {
        scan_id_def(ids, x);
    }
}

void scan_misc(Ids& ids, VeriTreeNode* x) {
    if (!ids.miscs.record(x)) {
        return;
    }

    if (auto nra = dynamic_cast<VeriNetRegAssign*>(x)) {
        scan_expression(ids, nra->GetLValExpr());
        scan_expression(ids, nra->GetRValExpr());
    } else {
        std::cerr << "unsupported TreeNode: " << typeid(*x).name() << "\n";
    }
}

void scan_miscs(Ids& ids, Array* a) {
    size_t i;
    VeriIdDef* x;
    FOREACH_ARRAY_ITEM(a, i, x) {
        scan_misc(ids, x);
    }
}
*/

/*

void encode_project(CborEncoder* ce) {
    Ids ids;

    encode_modules(ce, ids);
}

void encode_modules(CborEncoder* ce, Ids& ids) {
    MapIter map_iter;
    VeriModule* module = nullptr;

    FOREACH_VERILOG_MODULE(map_iter, module) {



        Array* items = module->GetModuleItems();
        std::cout << "module " << module->GetName() << ": " <<
            items->Size() << " items\n";

    }
}
*/

int main(int argc, char **argv) {
    for (uint32_t idx = 1; idx < argc; idx += 1) {
        if (!veri_file::Analyze(argv[idx], veri_file::SYSTEM_VERILOG)) {
            std::cerr << "failed to analyze " << argv[idx] << "\n";
            return 1;
        }
    }

    IdMap<VeriTreeNode> ids;
    Encoder enc;

    size_t len = 1024 * 1024;
    uint8_t* buf = (uint8_t*)malloc(len);
    cbor_encoder_init(enc.ce.get(), buf, len, 0);
    size_t init_used = cbor_encoder_get_buffer_size(enc.ce.get(), buf);
    std::cout << "initial: " << init_used << " bytes\n";
    encode_project(ids, enc);

    std::ofstream out("out.cbor", std::ofstream::binary);
    size_t used = cbor_encoder_get_buffer_size(enc.ce.get(), buf);
    std::cout << "generated " << used << " bytes\n";
    out.write((const char*)buf, used);
    assert(out);

    return 0;

    /*
    size_t len = 1024 * 1024;
    uint8_t* buf = malloc(len);
    assert(buf);
    CborEncoder ce;
    cbor_encoder_init(&ce, buf, 1024, 0);
    encode_project(&ce);

    size_t more = cbor_encoder_get_extra_bytes_needed(&ce);
    while (more > 0) {
        // Ran out of space in the buffer.  With `more` additional bytes, the
        // encoding would succeed.  Extend the buffer and try again.
        len += more;
        std::cout << "buffer too small - reallocating to " << len << "\n";
        buf = malloc(len);
        assert(buf);
        cbor_encoder_init(&ce, buf, 1024, 0);
        encode_project(&ce);

        more = cbor_encoder_get_extra_bytes_needed(&ce);
    }


    return 0;
    */
}
