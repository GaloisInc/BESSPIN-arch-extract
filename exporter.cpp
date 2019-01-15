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
#include <VeriScope.h>

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

    void double_(double value) {
        cbor_check(cbor_encode_floating_point(
                    ce.get(), CborDoubleType, (const void*)&value));
    }

    void string(const std::string& value) {
        cbor_check(cbor_encode_text_string(ce.get(), value.data(), value.size()));
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

// Cast `ptr` from `U*` to `T*` only if the dynamic type of `*ptr` is exactly
// `T`.  We use this in `encode_tree_node` to ensure we don't accidentally
// interpret an object as an instance of its superclass (losing information in
// any additional fields).
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
void encode_scope(IdMap<VeriTreeNode>& ids, Encoder& enc, VeriScope* s);
void encode_scope_map(IdMap<VeriTreeNode>& ids, Encoder& enc, Map* m);

static size_t num_unsupported = 0;

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
        encode_tree_nodes(ids, subenc, m->GetPorts());
        encode_tree_nodes(ids, subenc, m->GetParameters());
        encode_tree_nodes(ids, subenc, m->GetItems());
        encode_tree_nodes(ids, subenc, m->GetPortConnects());
        encode_tree_nodes(ids, subenc, m->GetParameterConnects());
        encode_tree_nodes(ids, subenc, m->GetPackageImportDecls());
    } else if (auto mi = exact_cast<VeriModuleInstantiation>(x)) {
        subenc.uint(ids.map(mi->GetInstantiatedModule()));
        encode_tree_nodes(ids, subenc, mi->GetParamValues());
        encode_tree_nodes(ids, subenc, mi->GetInstances());
    } else if (auto ii = exact_cast<VeriInstId>(x)) {
        subenc.string(ii->Name());
        encode_tree_nodes(ids, subenc, ii->GetPortConnects());
    } else if (auto dd = exact_cast<VeriDataDecl>(x)) {
        subenc.uint(dd->GetDeclType());
        subenc.uint(dd->GetDir());
        encode_tree_node(ids, subenc, dd->GetDataType());
        encode_tree_nodes(ids, subenc, dd->GetIds());
        assert(dd->GetResolutionFunction() == nullptr);
    } else if (auto nd = exact_cast<VeriNetDecl>(x)) {
        subenc.uint(nd->GetDeclType());
        subenc.uint(nd->GetDir());
        encode_tree_node(ids, subenc, nd->GetDataType());
        assert(nd->GetResolutionFunction() == nullptr);
        encode_tree_node(ids, subenc, nd->GetStrength());
        //encode_tree_nodes(ids, subenc, nd->GetDelay());
        encode_tree_nodes(ids, subenc, nd->GetIds());
    } else if (auto dt = exact_cast<VeriDataType>(x)) {
        subenc.uint(dt->GetType());
        subenc.uint(dt->GetSigning());
        encode_tree_node(ids, subenc, dt->GetDimensions());
    } else if (auto r = exact_cast<VeriRange>(x)) {
        encode_tree_node(ids, subenc, r->GetLeft());
        encode_tree_node(ids, subenc, r->GetRight());
        subenc.uint(r->GetPartSelectToken());
        subenc.uint(r->IsUnpacked());
        subenc.uint(r->LeftRangeBound());
        subenc.uint(r->RightRangeBound());
        encode_tree_node(ids, subenc, r->GetNext());
    } else if (auto cv = exact_cast<VeriConstVal>(x)) {
        subenc.string(cv->Image());
        subenc.uint(cv->Size(nullptr));
        subenc.uint(cv->Sign());
    } else if (auto iv = exact_cast<VeriIntVal>(x)) {
        subenc.string(iv->Image());
        subenc.uint(iv->Size(nullptr));
        subenc.uint(iv->Sign());
        subenc.int_(iv->GetNum());
    } else if (auto rv = exact_cast<VeriRealVal>(x)) {
        subenc.string(rv->Image());
        subenc.uint(rv->Size(nullptr));
        subenc.uint(rv->Sign());
        subenc.double_(rv->GetNum());
    } else if (auto v = exact_cast<VeriVariable>(x)) {
        subenc.string(v->Name());
        encode_tree_node(ids, subenc, v->GetDataType());
        encode_tree_node(ids, subenc, v->GetDimensions());
        encode_tree_node(ids, subenc, v->GetInitialValue());
        subenc.uint(v->Dir());
    } else if (auto uo = exact_cast<VeriUnaryOperator>(x)) {
        subenc.uint(uo->OperType());
        encode_tree_node(ids, subenc, uo->GetArg());
    } else if (auto bo = exact_cast<VeriBinaryOperator>(x)) {
        subenc.uint(bo->OperType());
        encode_tree_node(ids, subenc, bo->GetLeft());
        encode_tree_node(ids, subenc, bo->GetRight());
    } else if (auto ca = exact_cast<VeriContinuousAssign>(x)) {
        encode_tree_node(ids, subenc, ca->GetStrength());
        encode_tree_nodes(ids, subenc, ca->GetNetAssigns());
    } else if (auto nra = exact_cast<VeriNetRegAssign>(x)) {
        encode_tree_node(ids, subenc, nra->GetLValExpr());
        encode_tree_node(ids, subenc, nra->GetRValExpr());
    } else if (auto ir = exact_cast<VeriIdRef>(x)) {
        subenc.uint(ids.map(ir->GetId()));
    } else if (auto mi = exact_cast<VeriModuleId>(x)) {
        subenc.uint(ids.map(mi->GetModule()));
    } else if (auto ii = exact_cast<VeriIndexedId>(x)) {
        encode_tree_node(ids, subenc, ii->GetPrefix());
        encode_tree_node(ids, subenc, ii->GetIndexExpr());
        subenc.uint(ids.map(ii->GetId()));
    } else if (auto ac = exact_cast<VeriAlwaysConstruct>(x)) {
        encode_tree_node(ids, subenc, ac->GetStmt());
    } else if (auto ecs = exact_cast<VeriEventControlStatement>(x)) {
        encode_tree_nodes(ids, subenc, ecs->GetAt());
        encode_tree_node(ids, subenc, ecs->GetStmt());
    } else if (auto ee = exact_cast<VeriEventExpression>(x)) {
        subenc.uint(ee->GetEdgeToken());
        encode_tree_node(ids, subenc, ee->GetExpr());
        encode_tree_node(ids, subenc, ee->GetIffCondition());
    } else if (auto cs = exact_cast<VeriCaseStatement>(x)) {
        subenc.uint(cs->GetCaseStyle());
        subenc.uint(cs->GetCaseType());
        encode_tree_node(ids, subenc, cs->GetCondition());
        encode_tree_nodes(ids, subenc, cs->GetCaseItems());
    } else if (auto ci = exact_cast<VeriCaseItem>(x)) {
        encode_tree_nodes(ids, subenc, ci->GetConditions());
        encode_tree_node(ids, subenc, ci->GetStmt());
    } else if (auto ba = exact_cast<VeriBlockingAssign>(x)) {
        encode_tree_node(ids, subenc, ba->GetLVal());
        //encode_tree_node(ids, subenc, ba->GetControl());
        encode_tree_node(ids, subenc, ba->GetValue());
    } else if (auto nba = exact_cast<VeriNonBlockingAssign>(x)) {
        encode_tree_node(ids, subenc, nba->GetLVal());
        //encode_tree_node(ids, subenc, nba->GetControl());
        encode_tree_node(ids, subenc, nba->GetValue());
    } else if (auto cs = exact_cast<VeriConditionalStatement>(x)) {
        encode_tree_node(ids, subenc, cs->GetIfExpr());
        encode_tree_node(ids, subenc, cs->GetThenStmt());
        encode_tree_node(ids, subenc, cs->GetElseStmt());
    } else if (auto qc = exact_cast<VeriQuestionColon>(x)) {
        encode_tree_node(ids, subenc, qc->GetIfExpr());
        encode_tree_node(ids, subenc, qc->GetThenExpr());
        encode_tree_node(ids, subenc, qc->GetElseExpr());
    } else if (auto sb = exact_cast<VeriSeqBlock>(x)) {
        encode_tree_node(ids, subenc, sb->GetLabel());
        encode_tree_nodes(ids, subenc, sb->GetDeclItems());
        encode_tree_nodes(ids, subenc, sb->GetStatements());
    } else if (auto c = exact_cast<VeriConcat>(x)) {
        encode_tree_nodes(ids, subenc, c->GetExpressions());
    } else if (auto mc = exact_cast<VeriMultiConcat>(x)) {
        encode_tree_node(ids, subenc, mc->GetRepeat());
        encode_tree_nodes(ids, subenc, mc->GetExpressions());
    } else if (auto map = exact_cast<VeriMultiAssignmentPattern>(x)) {
        encode_tree_node(ids, subenc, map->GetTargetType());
        encode_tree_node(ids, subenc, map->GetRepeat());
        encode_tree_nodes(ids, subenc, map->GetExpressions());
    } else if (auto sc = exact_cast<VeriStreamingConcat>(x)) {
        subenc.uint(sc->OperType());
        encode_tree_nodes(ids, subenc, sc->GetExpressions());
        encode_tree_node(ids, subenc, sc->GetSliceSize());
    } else if (auto pc = exact_cast<VeriPortConnect>(x)) {
        subenc.string(pc->NamedFormal());
        encode_tree_node(ids, subenc, pc->GetConnection());
    } else if (auto tr = exact_cast<VeriTypeRef>(x)) {
        subenc.uint(ids.map(tr->GetId()));
    } else if (auto tr = exact_cast<VeriTypeId>(x)) {
        subenc.uint(ids.map(tr->GetModuleItem()));
    } else if (auto ds = exact_cast<VeriDotStar>(x)) {
        encode_scope(ids, subenc, ds->GetDotStarScope());
    } else if (auto e = exact_cast<VeriEnum>(x)) {
        encode_tree_node(ids, subenc, e->GetBaseType());
        encode_scope_map(ids, subenc, e->GetEnumLiterals());
    } else if (auto pi = exact_cast<VeriParamId>(x)) {
        encode_tree_node(ids, subenc, pi->GetDataType());
        encode_tree_node(ids, subenc, pi->GetInitialValue());
        subenc.uint(pi->ParamType());
        encode_tree_node(ids, subenc, pi->GetDimensions());
        encode_tree_node(ids, subenc, pi->GetActual());
    } else if (auto sn = exact_cast<VeriSelectedName>(x)) {
        subenc.uint(ids.map(sn->GetPrefixId()));
        subenc.string(sn->GetSuffix());
        subenc.uint(ids.map(sn->FullId()));
    } else if (auto f = exact_cast<VeriFor>(x)) {
        encode_tree_nodes(ids, subenc, f->GetInitials());
        encode_tree_node(ids, subenc, f->GetCondition());
        encode_tree_nodes(ids, subenc, f->GetRepetitions());
        encode_tree_node(ids, subenc, f->GetStmt());
    } else if (auto imi = exact_cast<VeriIndexedMemoryId>(x)) {
        encode_tree_node(ids, subenc, imi->GetPrefix());
        encode_tree_nodes(ids, subenc, imi->GetIndexes());
    } else if (auto ic = exact_cast<VeriInitialConstruct>(x)) {
        encode_tree_node(ids, subenc, ic->GetStmt());
    } else if (auto fd = exact_cast<VeriFunctionDecl>(x)) {
        encode_tree_node(ids, subenc, fd->GetId());
        encode_tree_node(ids, subenc, fd->GetDataType());
        encode_tree_nodes(ids, subenc, fd->GetAnsiIOList());
        encode_tree_nodes(ids, subenc, fd->GetDeclarations());
        encode_tree_nodes(ids, subenc, fd->GetStatements());
        encode_tree_nodes(ids, subenc, fd->GetPorts());
    } else if (auto fi = exact_cast<VeriFunctionId>(x)) {
        subenc.uint(ids.map(fi->GetModuleItem()));
    } else if (auto fc = exact_cast<VeriFunctionCall>(x)) {
        subenc.uint(ids.map(fc->GetId()));
        encode_tree_nodes(ids, subenc, fc->GetArgs());
    } else if (auto apd = exact_cast<VeriAnsiPortDecl>(x)) {
        subenc.uint(apd->GetDir());
        encode_tree_node(ids, subenc, apd->GetDataType());
        encode_tree_nodes(ids, subenc, apd->GetIds());
    } else if (auto d = exact_cast<VeriDollar>(x)) {
        subenc.string(d->Image());
    } else if (auto dcs = exact_cast<VeriDelayControlStatement>(x)) {
        encode_tree_node(ids, subenc, dcs->GetDelay());
        encode_tree_node(ids, subenc, dcs->GetStmt());
    } else if (auto ns = exact_cast<VeriNullStatement>(x)) {
    } else if (auto po = exact_cast<VeriPortOpen>(x)) {
    } else if (auto c = exact_cast<VeriCast>(x)) {
        encode_tree_node(ids, subenc, c->GetTargetType());
        encode_tree_node(ids, subenc, c->GetExpr());

    // Explicitly unsupported nodes
    } else if (auto ste = exact_cast<VeriSystemTaskEnable>(x)) {
    } else if (auto sfc = exact_cast<VeriSystemFunctionCall>(x)) {
    } else if (auto te = exact_cast<VeriTaskEnable>(x)) {

    } else {
        std::cerr << "unsupported AST node: " << typeid(*x).name() << "\n";
        ++num_unsupported;
    }
}

void encode_scope(IdMap<VeriTreeNode>& ids, Encoder& enc, VeriScope* s) {
    encode_scope_map(ids, enc, s->GetThisScope());
}

void encode_scope_map(IdMap<VeriTreeNode>& ids, Encoder& enc, Map* m) {
    SubEncoder entries(enc.array(m->Size()));

    MapIter map_iter;
    const char* name = nullptr;
    VeriIdDef* id_def = nullptr;

    FOREACH_MAP_ITEM(m, map_iter, &name, &id_def) {
        SubEncoder entry(entries.array(2));
        entry.string(name);
        entry.uint(ids.map(id_def));
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
    uint8_t* buf = new uint8_t[len];
    cbor_encoder_init(enc.ce.get(), buf, len, 0);
    encode_project(ids, enc);

    size_t more = cbor_encoder_get_extra_bytes_needed(enc.ce.get());
    if (more > 0) {
        len += more;
        delete[] buf;
        buf = new uint8_t[len];
        cbor_encoder_init(enc.ce.get(), buf, len, 0);
        encode_project(ids, enc);
    }

    std::ofstream out("out.cbor", std::ofstream::binary);
    size_t used = cbor_encoder_get_buffer_size(enc.ce.get(), buf);
    std::cout << "generated " << used << " bytes\n";
    out.write((const char*)buf, used);
    assert(out);

    std::cout << num_unsupported << " unsupported nodes\n";

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
