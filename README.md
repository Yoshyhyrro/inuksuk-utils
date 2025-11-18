# inuksuk-utils
A robust and thoughtful utility library for processing Inuktitut (Inuit language) text in Scheme. It provides essential tools for Unicode normalization, morphological abstraction, and basic linguistic analysis.

## Overview

This is a **proof-of-concept** implementation of a formal linguistic processing pipeline for Inuktitut, combining Hodge-inspired morphological decomposition with Kleene algebraic constraints for suffix generation. The project explores how polysynthetic language structures can be modeled using functional programming techniques in Scheme.

**Status: Experimental. Not production-ready.**

## Motivation

Inuktitut (ᐃᓄᒃᑦ), a polysynthetic language with complex morphological rules, presents unique challenges for natural language processing:

- **Morphological complexity**: A single word can encode concepts expressed as entire sentences in analytic languages
- **Suffix combinatorics**: Theoretically unbounded suffix concatenation requires formal constraints
- **Unicode representation**: Extended syllabics (U+18B0–U+18FF) coexist with basic syllabics (U+1400–U+167F), introducing normalization challenges
- **Multilingual co-existence**: Mixing Inuktitut with Latin-script languages (English, French) requires language-aware segmentation and processing

This project approaches these challenges using:
- **Hodge decomposition** for analyzing morphological depth and structural complexity
- **Kleene algebra** for formally constraining suffix chain generation
- **Configurable normalization pipelines** to handle Unicode and linguistic variation
- **Abstract syntax trees (ASTs)** for representing morphological structure

## Known Limitations

### Critical Limitations

1. **Not suitable for production use**
   - Lacks comprehensive testing on real corpus data
   - Error handling is basic (logging to stderr)
   - Performance characteristics undefined for large-scale processing

2. **Incomplete linguistic coverage**
   - Suffix-to-basic syllabic mappings are minimal placeholders
   - Canonical form collapsing is not implemented
   - No integration with authoritative Inuktitut lexicons or grammar databases

3. **Multilingual co-existence unsupported**
   - Current implementation assumes pure Inuktitut input
   - Mixing with English, French, or other Latin-script languages requires:
     - Language tagging/detection layer
     - Per-language pipeline branching
     - Context-aware segmentation
   - This is a **fundamental architectural limitation**, not a simple fix

4. **Kleene generation bounded artificially**
   - `max-depth` and `max-branches` prevent explosion but may miss valid forms
   - No linguistic validation of generated forms
   - Generated forms not checked against authoritative dictionaries

5. **Performance unvalidated**
   - No benchmarks for typical input sizes
   - Memory usage for large trees or many-rule sets unknown
   - FFI integration (ICU, HarfBuzz) not implemented; relies on basic Scheme utilities

### Design Trade-offs

- **Simplicity over completeness**: Prioritizes readability and clarity over handling all edge cases
- **Scheme over systems languages**: CHICKEN Scheme chosen for rapid prototyping; not optimized for speed
- **Formal over empirical**: Emphasizes algebraic structure over statistical/neural approaches
- **Sandboxed over integrated**: Runs independently; no integration with existing NLP pipelines

## Architecture

### Core Modules

1. **Error Handling & Logging** (`SECTION 1`)
   - Custom error types for Inuktitut-specific failures
   - Configurable log levels (debug, info, warning, error, critical)
   - Log buffer for post-processing analysis

2. **Unicode Normalization** (`SECTION 2`)
   - Zero-width character removal (U+200B, U+200C, U+200D, U+2060, U+FEFF)
   - Extended-to-basic syllabic folding (U+18B0–U+18FF → U+1400–U+167F)
   - Visual duplicate collapsing (placeholder for future lexicon-based approach)
   - Configurable normalization pipeline

3. **Hodge Decomposition** (`SECTION 3`)
   - Morphological depth analysis (shallow/moderate/deep)
   - Categorization by complexity and modification distance
   - Syllable counting specific to Inuktitut phonemics

4. **Kleene Algebra & Suffix Generation** (`SECTION 4`)
   - Suffix rule representation with linguistic constraints
   - Bounded suffix sequence generation
   - Branching constraint resolution
   - Safety limits to prevent exponential explosion

5. **Morphological Trees** (`SECTION 6`)
   - AST representation of word structure
   - Node types: root, stem, prefix, suffix, compound
   - Text-based tree visualization

6. **Performance Tracking** (`SECTION 7`)
   - Timing instrumentation
   - Resource limit configuration

### Data Flow

```
Input Text
    ↓
[Validation & Resource Checks]
    ↓
[Normalization Pipeline]
  (remove zero-width, fold extended, collapse duplicates)
    ↓
[Hodge Analysis]
  (depth classification, component extraction)
    ↓
[Morphological Tree Construction]
  (AST building from suffix derivation)
    ↓
Structured Analysis Result
```

## Usage

### Installation

Requires CHICKEN Scheme 5.x with standard libraries:
```bash
chicken-install irregex srfi-1 srfi-13
```

### Basic Example

```scheme
(import (chicken syntax) (chicken base))
(load "hodge-kleene-inuktitut.scm")

;; Simple normalization
(inuktitut-normalize "ᐃᓄᒃ")

;; Full analysis
(let ((analysis (inuktitut-analyze "ᐃᓄᒃᑦ")))
  (display (hodge-category analysis)))

;; Suffix generation
(let* ((rules (list
               (make-suffix-rule 'past "-ᐲᑦ" (lambda (x) #t) '(TENSE=PAST) '())))
       (forms (kleene-suffixes "ᑌ" rules #:optional 10 #:optional 100)))
  (display forms))
```

### Configuration

```scheme
;; Custom normalization config
(define my-norm-config
  (make-normalization-config #t #t #t #f))  ; remove-zw, fold-ext, collapse-dup, nfc-apply

;; Custom Hodge config
(define my-hodge-config
  (make-hodge-config 30 5 3))  ; polysynthetic-threshold, dirty-threshold, component-threshold

;; Custom resource limits
(define my-limits
  (make-resource-limits 10000 5000 100))  ; max input, max branches, max tree depth

(inuktitut-analyze text 
                   #:optional my-norm-config
                   #:optional my-hodge-config
                   #:optional my-limits)
```

## Project Goals

### Short Term
- [x] Implement core Hodge + Kleene pipeline
- [x] Add logging and error handling
- [ ] Comprehensive unit tests
- [ ] Documentation of linguistic assumptions

### Medium Term
- [ ] Integration with Inuktitut corpus data
- [ ] Validation against authoritative morphological dictionaries
- [ ] Performance benchmarking on realistic input
- [ ] FFI bindings to ICU (Unicode utilities) and HarfBuzz (text shaping)

### Long Term
- [ ] Multilingual pipeline (language detection + per-language branching)
- [ ] Integration with existing NLP toolkits (spaCy, NLTK, etc.)
- [ ] Browser extension/addon for real-world text processing
- [ ] Machine learning layer (classification, tagging)

## Contributing

We explicitly seek contributions from:

- **Inuktitut speakers and linguists**: Validate linguistic rules, expand lexicon mappings, and ensure cultural/linguistic accuracy
- **NLP engineers**: Optimize performance, add FFI integrations, design multilingual architecture
- **Formal language theorists**: Refine Hodge/Kleene formalization, explore alternative mathematical models
- **Unicode specialists**: Improve normalization strategies, handle edge cases

### How to Contribute

1. Open an issue describing the limitation, improvement, or insight
2. For linguists: Provide domain knowledge on suffix rules, morphological constraints, or canonical forms
3. For engineers: Submit pull requests with performance improvements, test cases, or architecture refinements
4. For researchers: Propose mathematical formalizations or alternative approaches

## Linguistic References

- Dorais, L.-J. (2010). "The Language of the Inuit: Syntax, Semantics, and Society in the Arctic"
- Fortescue, M. D. (1984). "West Greenlandic"
- Sadock, J. M. (1991). "Autolexical Syntax: A Theory of Parallel Grammatical Representations"
- UNICODE Standard, Section 15: Normalization (for Unicode handling)

## Technical References

- Kleene, S. C. (1956). "Representation of events in nerve nets and finite automata"
- Bao, Z. (1990). "On the Nature of Tone"
- Lehmann, W. P. (Ed.). (1978). "Syntactic Typology: Studies in the Phenomenology of Language"

## License

MIT License – See LICENSE file for details.

This allows free use, modification, and distribution, provided attribution is maintained and the license is included in derivative works.

## Disclaimer

**This project is experimental and academic in nature.** It is not:
- Endorsed by Inuktitut linguistic authorities
- Suitable for production language processing tasks
- A substitute for professional linguistic or translation services
- Guaranteed to produce accurate or complete morphological analysis

Users assume all responsibility for validating results against authoritative resources.

## Contact & Support

- **Issues & Discussions**: Use GitHub Issues for technical questions, bug reports, and linguistic insights
- **Community**: Engage with Inuktitut language communities and academic linguists for feedback
- **Citation**: If you use this work in research, please cite as:
  ```
  [Author(s)]. (2025). Hodge-Kleene Inuktitut Processor [Computer software]. GitHub.
  ```

## Acknowledgments

- The Inuktitut-speaking communities of Nunavut, Nunavik, and Inuit Tapiriit Kanatami
- Academic linguists specializing in polysynthetic and indigenous languages
- The CHICKEN Scheme community for tooling and libraries
- Contributors and collaborators (to be updated)

---

**Last Updated**: 2025-11-18  
**Status**: Proof of Concept – Seeking Community Input  
**Language**: Scheme (CHICKEN 5.x)
