# Changelog

All notable changes to the Scheme LLM Toolkit will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-08-05

### Added

#### Core Features
- **Provider Abstraction Layer**: Unified interface for multiple LLM providers
- **Ollama Integration**: Complete Ollama API client with streaming support
- **JSON Processing**: Custom JSON parser/generator with jq fallback
- **HTTP Client**: Robust HTTP utilities using curl subprocess
- **Functional Streaming**: Event-based streaming for real-time responses

#### Documentation
- Comprehensive architecture documentation with diagrams
- Technical blog post explaining design decisions
- Day-1 Scheme tutorial for LLM development
- Working examples in experiments/ directory

#### Build System
- Complete Makefile with dependency management
- Automated testing suite
- Cross-platform installation support
- FreeBSD compatibility

#### Modules Structure
- `(llm core provider)` - Provider abstraction layer
- `(llm providers ollama)` - Ollama API implementation  
- `(llm providers ollama-provider)` - Provider bridge for Ollama
- `(llm utils json)` - JSON processing utilities
- `(llm utils http)` - HTTP client utilities

### Fixed
- **Syntax Errors**: Resolved malformed shebang blocks in all experiment scripts
- **Compilation Issues**: Fixed unterminated comment errors preventing test execution
- **FreeBSD Compatibility**: Updated shebang to use `guile3` for proper execution

### Known Issues
- Segmentation fault in provider integration tests (under investigation)
- Manual guile-json installation required on some systems
- Ollama tests require running Ollama server

### Security
- Safe temporary file handling with proper cleanup
- No shell injection vulnerabilities in HTTP client
- Proper JSON escaping and validation

### Performance
- Efficient JSON parsing with custom implementation
- Streaming support for real-time LLM responses
- Minimal dependencies for fast startup

### Dependencies
- Guile 3.0+
- guile-json
- curl (for HTTP requests)
- Optional: jq (for enhanced JSON processing)

---

## Development Notes

This release represents the initial public version of the Scheme LLM Toolkit. The architecture is stable and the core functionality is working well. The toolkit provides a solid foundation for integrating Large Language Models into Scheme applications with proper functional programming patterns.

### Design Philosophy
- **Functional First**: All operations are pure functions that compose naturally
- **Provider Agnostic**: Core logic independent of specific LLM providers
- **Type Safe**: Strong typing through Scheme's type system
- **Streaming Focused**: Designed for real-time streaming with batch fallback
- **Error Recovery**: Comprehensive error handling with retry mechanisms

### Future Roadmap
- Additional provider implementations (OpenAI, Anthropic, Hugging Face)
- Enhanced streaming capabilities
- Vector store integration for RAG applications
- Performance optimizations and caching
- Extended documentation and tutorials