#include <algorithm>
#include <cassert>
#include <iostream>

#include <Array.h>
#include <Map.h>
#include <veri_file.h>
#include <VeriId.h>
#include <VeriExpression.h>
#include <VeriMisc.h>
#include <VeriModule.h>
#include <VeriStatement.h>

using namespace Verific;

int main(int argc, char **argv) {
    for (uint32_t idx = 1; idx < argc; idx += 1) {
        if (!veri_file::Analyze(argv[idx], veri_file::SYSTEM_VERILOG)) {
            std::cerr << "failed to analyze " << argv[idx] << "\n";
            return 1;
        }
    }

    MapIter map_iter;
    VeriModule* module = nullptr;

    FOREACH_VERILOG_MODULE(map_iter, module) {
        Array* items = module->GetModuleItems();
        std::cout << "module " << module->GetName() << ": " <<
            items->Size() << " items\n";

        size_t i;
        VeriModuleItem* item;
        FOREACH_ARRAY_ITEM(items, i, item) {
            if (!item->IsInstantiation()) {
                continue;
            }
            VeriModuleInstantiation* inst = (VeriModuleInstantiation*)item;
            std::cout << "  inst " << inst->GetModuleName() << "\n";

            size_t j;
            VeriInstId* id;
            FOREACH_ARRAY_ITEM(inst->GetInstances(), j, id) {
                std::cout << "    " << id->GetName() << "\n";

                size_t k;
                VeriExpression* conn;
                FOREACH_ARRAY_ITEM(id->GetPortConnects(), k, conn) {
                    std::cout << "        ";
                    conn->PrettyPrint(std::cout, 0);
                    std::cout << "\n";
                }
            }
        }
    }

    return 0;
}
