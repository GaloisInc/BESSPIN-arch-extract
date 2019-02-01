// This program uses Verific to parse a Verilog/SystemVerilog AST, then dumps
// out the AST in a CBOR-based encoding.
//
// The encoded AST format looks like this:
//
// At top level, there are two main sections: a list of nodes that make up the
// actual AST, and a list of file_info records that are used to map node spans
// back to their original source locations.
//
//      ast ::= [ [node*], [file_info*] ]
//
// The AST is not strictly tree structured - the same node may be accessible in
// multiple places, with none of them being "canonical".  To handle this, the
// exporter encodes the complete definition of each node at only one of its
// appearances in the AST, and elsewhere encodes only the node's unique ID.
// Null pointers are encoded as CBOR null.
//
//      node ::= node_def           (inline definition of a new node)
//             | node_id            (reference to a node defined elsewhere)
//             | null               (null pointer)
//
// Each node definition is a list with a header containing the node's ID and
// type.  The format of the remainder of the list depends on the node type.
//
//      node_def ::= [ node_id,     (ID of this node)
//                     class,       (node type)
//                     int,         (start offset - see below)
//                     int,         (end offset - see below)
//                     term* ]      (contents depends on node type)
//
//      node_id ::= integer
//
// Currently, node types are encoded using the mangled C++ name of the class.
//
//      class ::= string
//
// Source locations are encoded as pairs of 32-bit integers, giving the start
// and end byte offsets of the source span.  These byte offsets index into a
// virtual source string made by concatenating all source files that were used
// to build the AST.  The list of file_info objects allows mapping these global
// offsets to offsets within individual source files.
//
//      file_info ::= [ string,     (file name)
//                      int,        (start offset)
//                      int ]       (end offset)


#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <memory>
#include <vector>

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
#include <veri_yacc.h>

#include <tinycbor/cbor.h>

using namespace Verific;

template<typename T>
struct IdMap {
    std::map<const T*, uint32_t> obj_ids;
    uint32_t next_id;

    IdMap() : obj_ids(), next_id(1) {
        obj_ids.emplace(nullptr, 0);
    }

    std::pair<uint32_t, bool> map(const T* ptr) {
        auto iter = obj_ids.find(ptr);
        if (iter != obj_ids.end()) {
            return std::make_pair(iter->second, false);
        } else {
            uint32_t id = fresh();
            obj_ids.insert(std::make_pair(ptr, id));
            return std::make_pair(id, true);
        }
    }

    uint32_t fresh() {
        return next_id++;
    }
};

struct Span {
    uint32_t start;
    uint32_t end;

    Span() : start(0), end(0) {}
    Span(uint32_t start, uint32_t end) : start(start), end(end) {}
};

struct FileMap {
    // We reuse the string pointer provided by Verific, which is referenced by
    // a static `Map` and thus should be long-lived.
    const char* path;
    Span span;

    // Position (in sourcemap offsets) of each line start in this file.
    std::vector<uint32_t> line_starts;
};

struct SourceMap {
    std::vector<FileMap> files;
    // Keys are Verific string pointers (which are long-lived) - the same ones
    // used in FileMap.path.
    std::map<const char*, size_t> files_by_name;
    uint32_t next_start;

    SourceMap() : files(), files_by_name(), next_start(1) {}

    size_t add_file(const char* path) {
        std::ifstream in(path);

        FileMap fm;
        fm.path = path;
        fm.span.start = next_start;

        uint32_t off = 0;
        while (in) {
            std::string line;
            std::getline(in, line);
            fm.line_starts.push_back(fm.span.start + off);
            // `line` does not include the trailing `\n`
            off += line.size() + 1;
        }

        fm.span.end = fm.span.start + off;
        // The +1 prevents overlap between empty spans at the end of one file
        // and at the start of the next.
        next_start = fm.span.end + 1;

        files.emplace_back(std::move(fm));
        return files.size() - 1;
    }

    const FileMap& get_file(const char* path) {
        auto it = files_by_name.find(path);
        if (it != files_by_name.end()) {
            return files[it->second];
        } else {
            size_t idx = add_file(path);
            files_by_name.insert(it, std::make_pair(path, idx));
            return files[idx];
        }
    }

    Span convert(const ColLineFile* lf) {
        const FileMap& fm(get_file(lf->GetFileName()));
        uint32_t start_line = lf->GetLeftLine();
        uint32_t start_col = lf->GetLeftCol();
        uint32_t end_line = lf->GetRightLine();
        uint32_t end_col = lf->GetRightCol();
        uint32_t start = fm.line_starts.at(start_line - 1) + start_col - 1;
        uint32_t end = fm.line_starts.at(end_line - 1) + end_col - 1;
        return Span(start, end);
    }
};

void cbor_check(CborError err) {
    if (err == CborNoError || err == CborErrorOutOfMemory) {
        return;
    }
    std::cerr << "encoding error: " << err << "\n";
    abort();
}

// An `Encoder` writes terms sequentially into a CBOR array.  
struct Encoder {
    IdMap<VeriTreeNode>& ids;
    SourceMap& sm;
    std::unique_ptr<CborEncoder> ce;
    // Pointer to parent CborEncoder.  Used to close this `Encoder`'s array
    // when it is destroyed.
    CborEncoder* parent;

    // NB: `ce` must be manually initialized (`cbor_encoder_init`) before use.
    Encoder(IdMap<VeriTreeNode>& ids, SourceMap& sm, CborEncoder* parent=nullptr)
        : ids(ids), sm(sm), ce(new CborEncoder), parent(parent) {}
    Encoder(const Encoder&) = delete;
    Encoder(Encoder&& other) : ids(other.ids), sm(other.sm), ce(std::move(other.ce)), parent(other.parent) {}
    Encoder& operator=(const Encoder&) = delete;
    ~Encoder() {
        close();
    }

    void close() {
        if (ce && parent) {
            cbor_check(cbor_encoder_close_container(parent, ce.get()));
            ce.reset(nullptr);
            parent = nullptr;
        }
    }

    // Begin an array of length `len`, returning an `Encoder` that can be used
    // to insert elements into the new array.
    //
    // NB: it is not safe to use `this` until the array has been closed (by
    // destroying the sub-`Encoder`).
    Encoder array(size_t len=CborIndefiniteLength) {
        Encoder sub(ids, sm, ce.get());
        cbor_check(cbor_encoder_create_array(ce.get(), sub.ce.get(), len));
        return sub;
    }

    // Begin a map of length `len`, returning an `Encoder` that can be used to
    // insert keys and values into the new map.  Each pair of items encoded
    // into the map will result in a single key-value pair.  The total number
    // of items encoded should be twice `len` (if `len` is not indefinite).
    //
    // NB: it is not safe to use `this` until the array has been closed (by
    // destroying the sub-`Encoder`).
    Encoder map(size_t len=CborIndefiniteLength) {
        Encoder sub(ids, sm, ce.get());
        cbor_check(cbor_encoder_create_map(ce.get(), sub.ce.get(), len));
        return sub;
    }


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

    // Encode a single tree node, as a list `[id, cls, ...]`.  Encodes null if
    // the `x` pointer is null.
    void tree_node(VeriTreeNode* x);

    // Helper function to resolve and emit the list of port connections for a
    // module instantiation.
    void mod_inst_ports(VeriInstId* ii);

    // Encode an array of tree nodes.  Encodes an empty list if the `a` pointer
    // is null.
    void tree_nodes(Array* a) {
        if (a == nullptr) {
            // Start and then immediately close a new, empty array.
            this->array(0);
            return;
        }

        Encoder sub(this->array(a->Size()));
        size_t i;
        VeriTreeNode* x;
        FOREACH_ARRAY_ITEM(a, i, x) {
            sub.tree_node(x);
        }
    }

    // Encode a `VeriScope` as a map from names to nodes.
    void scope(VeriScope* s) {
        this->node_map(s->GetThisScope());
    }

    // Encode a `Map` from names to `VeriTreeNode`s.
    void node_map(Map* m) {
        Encoder sub(this->map(m->Size()));

        MapIter map_iter;
        const char* name = nullptr;
        VeriTreeNode* node = nullptr;

        FOREACH_MAP_ITEM(m, map_iter, &name, &node) {
            sub.string(name);
            sub.tree_node(node);
        }
    }

    void span(Span sp) {
        uint(sp.start);
        uint(sp.end);
    }

    void linefile(const ColLineFile* lf) {
        span(sm.convert(lf));
    }
};

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

static size_t num_unsupported = 0;

void Encoder::tree_node(VeriTreeNode* x) {
    if (x == nullptr) {
        this->null();
        return;
    }

    std::pair<uint32_t, bool> mapped_id = ids.map(x);
    uint32_t x_id = mapped_id.first;
    bool fresh = mapped_id.second;

    if (!fresh) {
        // For previously-encountered objects, simply encode the object's ID.
        this->uint(x_id);
        return;
    }

    Encoder sub(this->array());
    sub.uint(x_id);
    sub.string(typeid(*x).name());
    sub.linefile(x->Linefile());

    if (auto m = exact_cast<VeriModule>(x)) {
        sub.string(m->Name());
        sub.tree_node(m->GetId());
        sub.tree_nodes(m->GetPorts());
        sub.tree_nodes(m->GetParameters());
        sub.tree_nodes(m->GetItems());
        sub.tree_nodes(m->GetPortConnects());
        sub.tree_nodes(m->GetParameterConnects());
        sub.tree_nodes(m->GetPackageImportDecls());
    } else if (auto mi = exact_cast<VeriModuleInstantiation>(x)) {
        sub.tree_node(mi->GetInstantiatedModule());
        // TODO: handle params like InstId ports
        sub.tree_nodes(mi->GetParamValues());
        sub.tree_nodes(mi->GetInstances());
    } else if (auto ii = exact_cast<VeriInstId>(x)) {
        sub.tree_node(ii->GetModuleItem());
        sub.string(ii->Name());
        sub.mod_inst_ports(ii);
    } else if (auto dd = exact_cast<VeriDataDecl>(x)) {
        sub.uint(dd->GetDeclType());
        sub.uint(dd->GetDir());
        sub.tree_node(dd->GetDataType());
        sub.tree_nodes(dd->GetIds());
        assert(dd->GetResolutionFunction() == nullptr);
    } else if (auto nd = exact_cast<VeriNetDecl>(x)) {
        sub.uint(nd->GetDeclType());
        sub.uint(nd->GetDir());
        sub.tree_node(nd->GetDataType());
        assert(nd->GetResolutionFunction() == nullptr);
        sub.tree_node(nd->GetStrength());
        //sub.tree_nodes(nd->GetDelay());
        sub.tree_nodes(nd->GetIds());
    } else if (auto dt = exact_cast<VeriDataType>(x)) {
        sub.uint(dt->GetType());
        sub.uint(dt->GetSigning());
        sub.tree_node(dt->GetDimensions());
    } else if (auto r = exact_cast<VeriRange>(x)) {
        sub.tree_node(r->GetLeft());
        sub.tree_node(r->GetRight());
        sub.uint(r->GetPartSelectToken());
        sub.uint(r->IsUnpacked());
        sub.uint(r->LeftRangeBound());
        sub.uint(r->RightRangeBound());
        sub.tree_node(r->GetNext());
    } else if (auto cv = exact_cast<VeriConstVal>(x)) {
        sub.string(cv->Image());
        sub.uint(cv->Size(nullptr));
        sub.uint(cv->Sign());
    } else if (auto iv = exact_cast<VeriIntVal>(x)) {
        sub.string(iv->Image());
        sub.uint(iv->Size(nullptr));
        sub.uint(iv->Sign());
        sub.int_(iv->GetNum());
    } else if (auto rv = exact_cast<VeriRealVal>(x)) {
        sub.string(rv->Image());
        sub.uint(rv->Size(nullptr));
        sub.uint(rv->Sign());
        sub.double_(rv->GetNum());
    } else if (auto v = exact_cast<VeriVariable>(x)) {
        sub.string(v->Name());
        sub.tree_node(v->GetDataType());
        sub.tree_node(v->GetDimensions());
        sub.tree_node(v->GetInitialValue());
        sub.uint(v->Dir());
    } else if (auto uo = exact_cast<VeriUnaryOperator>(x)) {
        sub.uint(uo->OperType());
        sub.tree_node(uo->GetArg());
    } else if (auto bo = exact_cast<VeriBinaryOperator>(x)) {
        sub.uint(bo->OperType());
        sub.tree_node(bo->GetLeft());
        sub.tree_node(bo->GetRight());
    } else if (auto ca = exact_cast<VeriContinuousAssign>(x)) {
        sub.tree_node(ca->GetStrength());
        sub.tree_nodes(ca->GetNetAssigns());
    } else if (auto nra = exact_cast<VeriNetRegAssign>(x)) {
        sub.tree_node(nra->GetLValExpr());
        sub.tree_node(nra->GetRValExpr());
    } else if (auto ir = exact_cast<VeriIdRef>(x)) {
        // NB: If you change the encoding of `VeriIdRef`s, also update
        // `mod_inst_ports` to generate the new encoding.
        sub.tree_node(ir->GetId());
    } else if (auto mi = exact_cast<VeriModuleId>(x)) {
        sub.tree_node(mi->GetModule());
    } else if (auto ii = exact_cast<VeriIndexedId>(x)) {
        sub.tree_node(ii->GetPrefix());
        sub.tree_node(ii->GetIndexExpr());
        sub.tree_node(ii->GetId());
    } else if (auto ac = exact_cast<VeriAlwaysConstruct>(x)) {
        sub.tree_node(ac->GetStmt());
        unsigned kind = VERI_ALWAYS;
        if (ac->GetQualifier(VERI_ALWAYS_COMB)) {
            kind = VERI_ALWAYS_COMB;
        } else if (ac->GetQualifier(VERI_ALWAYS_LATCH)) {
            kind = VERI_ALWAYS_LATCH;
        } else if (ac->GetQualifier(VERI_ALWAYS_FF)) {
            kind = VERI_ALWAYS_FF;
        }
        sub.uint(kind);
    } else if (auto ecs = exact_cast<VeriEventControlStatement>(x)) {
        sub.tree_nodes(ecs->GetAt());
        sub.tree_node(ecs->GetStmt());
    } else if (auto ee = exact_cast<VeriEventExpression>(x)) {
        sub.uint(ee->GetEdgeToken());
        sub.tree_node(ee->GetExpr());
        sub.tree_node(ee->GetIffCondition());
    } else if (auto cs = exact_cast<VeriCaseStatement>(x)) {
        sub.uint(cs->GetCaseStyle());
        sub.uint(cs->GetCaseType());
        sub.tree_node(cs->GetCondition());
        sub.tree_nodes(cs->GetCaseItems());
    } else if (auto ci = exact_cast<VeriCaseItem>(x)) {
        sub.tree_nodes(ci->GetConditions());
        sub.tree_node(ci->GetStmt());
    } else if (auto ba = exact_cast<VeriBlockingAssign>(x)) {
        sub.tree_node(ba->GetLVal());
        sub.tree_node(ba->GetControl());
        sub.tree_node(ba->GetValue());
        sub.uint(ba->OperType());
    } else if (auto nba = exact_cast<VeriNonBlockingAssign>(x)) {
        sub.tree_node(nba->GetLVal());
        sub.tree_node(nba->GetControl());
        sub.tree_node(nba->GetValue());
    } else if (auto cs = exact_cast<VeriConditionalStatement>(x)) {
        sub.tree_node(cs->GetIfExpr());
        sub.tree_node(cs->GetThenStmt());
        sub.tree_node(cs->GetElseStmt());
    } else if (auto qc = exact_cast<VeriQuestionColon>(x)) {
        sub.tree_node(qc->GetIfExpr());
        sub.tree_node(qc->GetThenExpr());
        sub.tree_node(qc->GetElseExpr());
    } else if (auto sb = exact_cast<VeriSeqBlock>(x)) {
        sub.tree_node(sb->GetLabel());
        sub.tree_nodes(sb->GetDeclItems());
        sub.tree_nodes(sb->GetStatements());
    } else if (auto c = exact_cast<VeriConcat>(x)) {
        sub.tree_nodes(c->GetExpressions());
    } else if (auto mc = exact_cast<VeriMultiConcat>(x)) {
        sub.tree_node(mc->GetRepeat());
        sub.tree_nodes(mc->GetExpressions());
    } else if (auto map = exact_cast<VeriMultiAssignmentPattern>(x)) {
        sub.tree_node(map->GetTargetType());
        sub.tree_node(map->GetRepeat());
        sub.tree_nodes(map->GetExpressions());
    } else if (auto sc = exact_cast<VeriStreamingConcat>(x)) {
        sub.uint(sc->OperType());
        sub.tree_nodes(sc->GetExpressions());
        sub.tree_node(sc->GetSliceSize());
    } else if (auto pc = exact_cast<VeriPortConnect>(x)) {
        sub.string(pc->NamedFormal());
        sub.tree_node(pc->GetConnection());
    } else if (auto tr = exact_cast<VeriTypeRef>(x)) {
        sub.tree_node(tr->GetId());
    } else if (auto ti = exact_cast<VeriTypeId>(x)) {
        sub.string(ti->Name());
        sub.tree_node(ti->GetDataType());
    } else if (auto ds = exact_cast<VeriDotStar>(x)) {
        sub.scope(ds->GetDotStarScope());
    } else if (auto e = exact_cast<VeriEnum>(x)) {
        sub.tree_node(e->GetBaseType());
        sub.node_map(e->GetEnumLiterals());
    } else if (auto pi = exact_cast<VeriParamId>(x)) {
        sub.string(pi->Name());
        sub.tree_node(pi->GetDataType());
        sub.tree_node(pi->GetInitialValue());
        sub.uint(pi->ParamType());
        sub.tree_node(pi->GetDimensions());
        sub.tree_node(pi->GetActual());
    } else if (auto sn = exact_cast<VeriSelectedName>(x)) {
        sub.tree_node(sn->GetPrefix());
        sub.string(sn->GetSuffix());
        sub.tree_node(sn->FullId());
    } else if (auto f = exact_cast<VeriFor>(x)) {
        sub.tree_nodes(f->GetInitials());
        sub.tree_node(f->GetCondition());
        sub.tree_nodes(f->GetRepetitions());
        sub.tree_node(f->GetStmt());
    } else if (auto imi = exact_cast<VeriIndexedMemoryId>(x)) {
        sub.tree_node(imi->GetPrefix());
        sub.tree_nodes(imi->GetIndexes());
    } else if (auto ic = exact_cast<VeriInitialConstruct>(x)) {
        sub.tree_node(ic->GetStmt());
    } else if (auto fd = exact_cast<VeriFunctionDecl>(x)) {
        sub.tree_node(fd->GetId());
        sub.tree_node(fd->GetDataType());
        sub.tree_nodes(fd->GetAnsiIOList());
        sub.tree_nodes(fd->GetDeclarations());
        sub.tree_nodes(fd->GetStatements());
        sub.tree_nodes(fd->GetPorts());
    } else if (auto fi = exact_cast<VeriFunctionId>(x)) {
        sub.tree_node(fi->GetModuleItem());
    } else if (auto fc = exact_cast<VeriFunctionCall>(x)) {
        sub.tree_node(fc->GetId());
        sub.tree_nodes(fc->GetArgs());
    } else if (auto apd = exact_cast<VeriAnsiPortDecl>(x)) {
        sub.uint(apd->GetDir());
        sub.tree_node(apd->GetDataType());
        sub.tree_nodes(apd->GetIds());
    } else if (auto d = exact_cast<VeriDollar>(x)) {
        sub.string(d->Image());
    } else if (auto dcs = exact_cast<VeriDelayControlStatement>(x)) {
        sub.tree_node(dcs->GetDelay());
        sub.tree_node(dcs->GetStmt());
    } else if (auto ns = exact_cast<VeriNullStatement>(x)) {
    } else if (auto po = exact_cast<VeriPortOpen>(x)) {
    } else if (auto c = exact_cast<VeriCast>(x)) {
        sub.tree_node(c->GetTargetType());
        sub.tree_node(c->GetExpr());

    // Explicitly unsupported nodes
    } else if (auto ste = exact_cast<VeriSystemTaskEnable>(x)) {
    } else if (auto sfc = exact_cast<VeriSystemFunctionCall>(x)) {
    } else if (auto te = exact_cast<VeriTaskEnable>(x)) {

    } else {
        std::cerr << "unsupported AST node: " << typeid(*x).name() << "\n";
        ++num_unsupported;
    }
}

void Encoder::mod_inst_ports(VeriInstId* ii) {
    // We want to encode an array of the actual ports that matches the order of
    // the corresponding formal ports.  Verific provides a function for
    // obtaining the actual parameter corresponding to a formal, but that
    // function dosen't handle `.*`.

    VeriModule* module = ii->GetInstantiatedModule();

    if (module == nullptr) {
        VeriModuleInstantiation* mi = ii->GetModuleInstance();
        std::cerr << "warning: module instantiation " << ii->Name()
            << " references undefined module "
            << (mi ? mi->GetModuleName() : "(null)")
            << "\n";

        Array* conns = ii->GetPortConnects();
        // Indefinite array length since we may skip over some elements
        // (namely, any `DotStar`s).
        Encoder sub(this->array());
        size_t i;
        VeriExpression* e;
        FOREACH_ARRAY_ITEM(conns, i, e) {
            if (auto pc = exact_cast<VeriPortConnect>(e)) {
                e = pc->GetConnection();
            }
            if (typeid(*e) == typeid(VeriDotStar)) {
                std::cerr << "warning: module instantiation " << ii->Name()
                    << " uses `.*`, but module declaration port list is unavailable\n";
                continue;
            }
            sub.tree_node(e);
        }
        return;
    }

    // Walk over the formals, encoding each corresponding actual.  If the
    // lookup of the actual returns either .* or NULL, we look in the enclosing
    // scope for an identifier with a matching name.

    Array* formal_ports = module->GetPorts();
    Encoder sub(this->array(formal_ports->Size()));
    size_t i;
    VeriIdDef* formal;
    FOREACH_ARRAY_ITEM(formal_ports, i, formal) {
        VeriExpression* actual = ii->GetActualExpression(formal);
        if (actual != nullptr && typeid(*actual) != typeid(VeriDotStar)) {
            sub.tree_node(actual);
        } else {
            VeriIdDef* actual_id = ii->GetOwningScope()->Find(formal->Name());
            // This is a bit ugly - we emit a fake VeriIdRef "by hand".
            Encoder fake_ref(sub.array());
            fake_ref.uint(ids.fresh());
            fake_ref.string(typeid(VeriIdRef).name());
            fake_ref.span(Span(0, 0));
            fake_ref.tree_node(actual_id);
            fake_ref.close();
        }
    }
}

void encode_project(Encoder& enc) {
    Map* modules = veri_file::AllModules();

    Encoder top(enc.array());

    {
        Encoder ast(top.array());

        MapIter map_iter;
        const char* name = nullptr;
        VeriModule* module = nullptr;

        FOREACH_MAP_ITEM(modules, map_iter, &name, &module) {
            ast.tree_node(module);
        }
    }

    {
        Encoder file_infos(top.array());
        for (const auto& fm : enc.sm.files) {
            Encoder sub(file_infos.array());
            sub.string(fm.path);
            sub.span(fm.span);
        }
    }
}

int main(int argc, char **argv) {
    for (uint32_t idx = 1; idx < argc; idx += 1) {
        if (!veri_file::Analyze(argv[idx], veri_file::SYSTEM_VERILOG)) {
            std::cerr << "failed to analyze " << argv[idx] << "\n";
            return 1;
        }
    }

    IdMap<VeriTreeNode> ids;
    SourceMap sm;
    Encoder enc(ids, sm);

    size_t len = 1024 * 1024;
    uint8_t* buf = new uint8_t[len];
    cbor_encoder_init(enc.ce.get(), buf, len, 0);
    encode_project(enc);

    size_t more = cbor_encoder_get_extra_bytes_needed(enc.ce.get());
    if (more > 0) {
        len += more;
        delete[] buf;
        buf = new uint8_t[len];
        cbor_encoder_init(enc.ce.get(), buf, len, 0);
        encode_project(enc);
    }

    std::ofstream out("out.cbor", std::ofstream::binary);
    size_t used = cbor_encoder_get_buffer_size(enc.ce.get(), buf);
    std::cout << "generated " << used << " bytes\n";
    out.write((const char*)buf, used);
    assert(out);

    std::cout << num_unsupported << " unsupported nodes\n";

    return 0;
}
