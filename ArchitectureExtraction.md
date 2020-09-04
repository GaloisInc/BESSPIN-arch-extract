# Architecture Extraction for Hardware Description Languages

One of the capabilities developed by Galois under the BESSPIN project is
*architecture extraction*, an automated process for converting source code
into simpler representations that preserve structure. An extracted
architecture is primarily intended to be easy to visualize, while conveying
meaningful high-level information about the implementation. In BESSPIN,
architecture extraction has been implemented for the high-level hardware
description languages *Chisel* (an extension of the general-purpose language
Scala) and *Bluespec SystemVerilog* (BSV), as well as the synthesizable
SystemVerilog HDL to which both Chisel and BSV are compiled. Each of these
source languages is parsed and translated into Graphviz source representing
data flow and module dependency structure, with a few configurable heuristics
for simplification.

This document is a brief overview of the architecture extraction concept and
our current implementation of it, as well as an initial exploration of the
feasibility and potential value of translating SystemVerilog source into
either of two more sophisticated architecture languages: *Statechart XML*
(SCXML) and the SAE *Architecture Analysis and Design Language* (AADL).


- concept
- current implementation
    - examples
    - limitations
- SCXML
- AADL
- related work
- proposal


## Concept

The purpose of the BESSPIN project is to develop software tools and design
methods that can bring higher levels of security assurance and better risk
mitigation to systems engineering workflows which must explicitly include
hardware components within a trusted computing base. More specifically, the
BESSPIN Tool Suite workflow is intended to allow engineers to assess novel
hardware-based solutions to software security problems in a comprehensive,
quantitative, comparative, and reproducible framework. A typical use case for
the BESSPIN Tool Suite will include the assessment of hardware definition
source code developed by third parties. Such HDL code bases are likely to
include parameters configurable during compilation, which may have complex
interdependencies and interactions, entailing unknown effects on both
performance and security. The documentation which accompanies such HDL sources
can not be assumed complete or reliable with respect to security properties.
Thus, the BESSPIN tools derive various measurements and assessments directly
from HDL source code, as well as static analyses of software artifacts and
dynamic property testing of combined hardware-software systems.

Architecture extraction is one of the first steps in the BESSPIN Tool Suite
workflow. It consumes an HDL project file structure and produces diagrams
showing how the project's modules and their constituent logic elements and
registers are interconnected.


## Current implementation

The architecture extraction capability provided by the BESSPIN Tool Suite
shares a code repository, control script, and configuration format with -- but
is otherwise largely independent of -- an implementation of *feature model
extraction*: a related capability which generates a model of an HDL project's
configurable build parameters and their logical interdependencies. 
Architecture extraction itself is implemented as a pipeline process consisting
of several parser modules (one for each supported source language) which share
a common output format, and a translation module. The intermediate
representation produced by the parser is consumed by the translator, which can
be configured to simplify its output by collapsing or omitting certain kinds
of connections or elements in the source design. Ultimately, the translator
produces Graphviz source text, which in turn may be rendered into diagrams.


### Examples

Consider this SystemVerilog implementation of a very simple design with two modules:

    module adder #(width = 8) (
        input logic [width - 1:0] a,
        input logic [width - 1:0] b,
        output logic [width - 1:0] c
    );

    assign c = a + b;

    endmodule


    module tinymult #(width = 8) (
        input logic [width - 1:0] a,
        input logic [1:0] b,
        output logic [width - 1:0] c
    );

    logic [width - 1:0] s1 = b[0] ? a : 0;
    logic [width - 1:0] s2 = b[1] ? a << 1 : 0;

    adder #(width) add(s1, s2, c);

    endmodule

The most detailed diagram that the architecture extraction tool can produce for this design is

**TODO: output png(s?) here...**

and the least detailed diagram is

**TODO: ...and here**

*I'm pretty sure I at least had the driver script working with basic examples when I put it down,
a couple of months ago.
Not sure what happened since then, but the README is out of date
and the nix shell no longer works for me. 
I spent some time today trying to get it going again on various tool-suite branches,
but ran into a few different forms of bitrot, and gave up after a while. --MO*

The options which control the extraction tool are these:

**TODO:**
*(documented in the .toml files in the examples dir and the BESSPIN/ArchExtract/Config.hs source)* 


### Limitations

*I honestly don't even know where to start with this.
Maybe with unsupported Verilog language features?
Going deeper would really depend on a specific use case.
--MO*


## SCXML

Originally [invented by David Harel](http://www.wisdom.weizmann.ac.il/~dharel/SCANNED.PAPERS/Statecharts.pdf)
in the early 1980s, a *statechart* is essentially a hierarchically scoped state machine
extended with notions of concurrency and communication.
They are used to describe reactive (event-driven) systems.
The *Statechart XML* [specification](https://www.w3.org/TR/scxml/) produced by the W3C
standardizes and formalizes the statechart concept, providing a syntax and an execution environment.
As with so many things XML, there is a more recent JSON-based implementation of the same API,
called [XState](https://xstate.js.org/docs/about/goals.html).
There is a small ecosystem of statechart visualization, testing, and development tools
typically oriented toward user interface development,
although of course the formalism is much more broadly applicable.
Some of these are linked from https://statecharts.github.io/.

**TODO:**
*(Prospects for HDL extraction, potential issues.
State machine hierarchy is somewhat orthogonal to component hierarchy.
Good first-order conceptual match for event-driven Verilog, but lots of details to consider.)*


## AADL

The [*Architecture Analysis and Design Language*](https://en.wikipedia.org/wiki/Architecture_Analysis_%26_Design_Language)
specified by [SAE](https://www.sae.org/standards/content/as5506c/),
originally developed at Honeywell,
and currently supported by the CMU
[Software Engineering Institute](https://www.sei.cmu.edu/research-capabilities/all-work/display.cfm?customel_datapageid_4050=191439)
is intended as a unified hardware and software modeling language for real-time embedded systems,
with an historical emphasis on vehicles.
It expresses component hierarchies, synchronous communication channels,
and user-defined properties which can reference either of these
entities as well as arbitrary variables.
It has been extended with "annex" languages oriented toward specific analytic techniques,
and has a small ecosystem including [OSATE](https://osate.org/), an Eclipse-based editor
with a graphical mode.

**TODO:**
*(Prospects for extraction, potential issues.
Superficially looks similar to existing implementation.
Value add? Conceptual mismatches with HDL? Limitations?)*


## Related work

*I had plans to do an open-access literature survey, but didn't get very far.
I'm not sure how relevant any of these are, or what I've missed. --MO*

- [Stéphane Ducasse, Damien Pollet. Software Architecture Reconstruction: A Process-Oriented Taxonomy. IEEE Transactions on Software Engineering, Institute of Electrical and Electronics Engineers, 2009, ￿10.1109/TSE.2009.19￿.  inria-00498407](https://hal.inria.fr/inria-00498407/document)
- [Multiple Viewpoints Architecture Extraction](https://www.researchgate.net/profile/Herve_Verjus/publication/224605932_Multiple_viewpoints_architecture_extraction/links/0fcfd50bfaaf7a80e4000000/Multiple-viewpoints-architecture-extraction.pdf)
- Chardigny, Sylvain, et al. "Search-based extraction of component-based architecture from object-oriented systems." European Conference on Software Architecture. Springer, Berlin, Heidelberg, 2008. *(XXX: looks paywalled again*)
- Yuanfang Cai, Hanfei Wang, Sunny Wong, and Linzhang Wang. 2013. Leveraging design rules to improve software architecture recovery. In Proceedings of the 9th international ACM Sigsoft conference on Quality of software architectures (QoSA '13). Association for Computing Machinery, New York, NY, USA, 133–142. DOI:https://doi.org/10.1145/2465478.2465480 *(XXX: ditto)*


## Proposal

*In case it's not immediately obvious, this is just a very rough sketch of some ideas.
More like a brain dump. --MO*

Don't call it "architecture" extraction, call it "model" extraction.
*Architecture* is an overloaded term in computer hardware engineering,
and a somewhat contentious term in software engineering.
The major problem I see is that it refers to *intent* of the original designers,
which is an implicit quality, highly subjective, and just generally not something
that can be automatically reconstructed.
So using that term sort of gets us off on the wrong foot.
A *model*, on the other hand, is a tangible and relatively value-neutral artifact.

Simply tracing out the module structure of a project
(as in the present implementation)
is conceptually straightforward, but adds little value.
Human analysts or downstream engineers can determine the module structure
directly from the code itself by reading it, as can
automated static analysis tools.
Whether we think of an extracted model as a simplified
"intermediate representation" target for analysis tools
or for direct human consumption, it should represent
properties or perspectives that are not immediately apparent
from the source code.

Think of model extraction as an interactive process, with a human in the loop.
We are *simplifying while retaining meaning*:
ideally throwing away all the uninteresting information and none of the good stuff.
This is really hard! Automation can aid consistency,
but enforcing consistency is about the easiest part of the challenge.
For any given design and analysis, there are many choices to be made along the way,
typically with no objective basis by which to make them represented in the design itself.
Requiring the user to specify all these considerations ahead of time
just makes for a frustrating and slow user experience.
But, keep track of the user's choices so they can be replayed automatically
when extracting a new model from a related design.

Use an existing modeling language as a target. Don't roll your own.
There are plenty. Embrace and extend one if you really have to. 
It should be semantically rich enough to express interesting properties.
Ideally it would have a tooling ecosystem and some industrial adoption.
Graphviz has the latter qualities but none of the former;
it is much too unstructured and display-oriented.
Either of AADL or SCXML above may be useful, although for different purposes;
they are very different representations, neither of which
were intended for circuit-level digital hardware.
Maybe there's a better modeling language out there, too.
Depends on what kind of analyses you're interested in doing.

Even if this is going to be an IR&D thing,
we should find a prospective customer with a specific need, early in the process.
Otherwise the whole concept is just too broad and speculative.

Involve actual hardware experts, with training and industry experience,
throughout the tool design and implementation process.
Make them actually use development versions, and listen to their feedback.
Prioritize their concerns over your own.
Ditto for computer security experts, if it's going to be a security thing.
My impression is that hardware engineering is already a more mature discipline
than software engineering.
I might be wrong about that, or missed the point or something,
but there's a more general principle I'm pretty sure of:
assuming that "we" can do something meaningful for "them"
*without their participation*
isn't merely hubris, it's chutzpah.
Just because this attitude is endemic to the software field doesn't make it OK.
Anyway, it makes the project and finished product unlikely to have any lasting impact.
Oops, am I ranting? Guess I'm done then.
Good luck!
