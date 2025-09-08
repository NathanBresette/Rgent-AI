# Contributing to RgentAI

We welcome contributions to RgentAI! This document outlines how to contribute to the project and get support.

## How to Contribute

### Reporting Issues
- **Check existing issues first** to avoid duplicates
- **Use GitHub Issues** to report bugs or request features
- **Include a clear, descriptive title** and detailed description
- **Provide reproducible examples** when reporting bugs
- **Include system information**: R version, OS, package versions
- **Describe expected vs actual behavior**

### Suggesting Enhancements
- **Use GitHub Issues** with the "enhancement" label
- **Clearly describe the enhancement** and its benefits
- **Explain why this would be useful** to other users
- **Consider backward compatibility** implications

### Code Contributions

#### Getting Started
1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/yourusername/rstudioai.git
   cd rstudioai
   ```
3. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

#### Development Setup
1. **Install development dependencies**:
   ```r
   install.packages(c("devtools", "roxygen2", "testthat"))
   ```
2. **Load the package** for development:
   ```r
   devtools::load_all()
   ```
3. **Run tests**:
   ```r
   devtools::test()
   ```

#### Making Changes
- **Follow R coding standards** and existing code style
- **Add or update documentation** for any new functions
- **Include tests** for new functionality
- **Update NEWS.md** for user-facing changes
- **Keep commits atomic** and write clear commit messages

#### Code Style Guidelines
- Use **snake_case** for function and variable names
- Use **descriptive names** that clearly indicate purpose
- **Comment complex logic** and document functions using roxygen2
- **Limit line length** to 80 characters where possible
- **Use consistent indentation** (2 spaces)

#### Testing
- **Write tests** for all new functionality using testthat
- **Ensure all tests pass** before submitting
- **Include edge cases** and error conditions in tests
- **Test on multiple R versions** if possible

#### Documentation
- **Document all exported functions** using roxygen2
- **Include examples** in function documentation
- **Update README.md** if adding major features
- **Write clear, concise descriptions**

### Submitting Changes
1. **Ensure all tests pass**:
   ```r
   devtools::check()
   ```
2. **Commit your changes**:
   ```bash
   git add .
   git commit -m "Add descriptive commit message"
   ```
3. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```
4. **Create a Pull Request** on GitHub

#### Pull Request Guidelines
- **Provide a clear description** of the changes
- **Reference related issues** using keywords (e.g., "Fixes #123")
- **Include tests** for new functionality
- **Update documentation** as needed
- **Be responsive** to feedback and requests for changes

## Getting Support

### Documentation
- **Read the README** for basic usage information
- **Check function documentation**: `?function_name`
- **Browse the issues** for similar questions

### Asking Questions
- **Use GitHub Discussions** for general questions
- **Use GitHub Issues** for bug reports and feature requests
- **Include reproducible examples** when asking for help
- **Be specific** about what you're trying to achieve

### Contact
- **Email**: [your-email@domain.com]
- **GitHub Issues**: For bugs and feature requests
- **GitHub Discussions**: For questions and general discussion

## Development Workflow

### Agent Development
When working on agent functionality:
1. **Understand the agent architecture** in `R/agents/`
2. **Follow existing patterns** for consistency
3. **Test with multiple data types** and edge cases
4. **Document method selection logic** clearly

### UI Development
When working on the web interface:
1. **Test in RStudio Viewer** pane
2. **Ensure responsive design** works on different screen sizes
3. **Follow existing JavaScript patterns**
4. **Test WebSocket functionality** thoroughly

### Backend Integration
When working on AI service integration:
1. **Handle network failures gracefully**
2. **Provide meaningful error messages**
3. **Test with and without access codes**
4. **Maintain backward compatibility**

## Code of Conduct

### Our Standards
- **Be respectful** and inclusive in all interactions
- **Welcome newcomers** and help them learn
- **Focus on what is best** for the community
- **Show empathy** towards other community members

### Unacceptable Behavior
- Harassment, discrimination, or offensive language
- Personal attacks or trolling
- Spam or self-promotion unrelated to the project
- Any other conduct that could reasonably be considered inappropriate

### Enforcement
Instances of unacceptable behavior may be reported by contacting the project maintainers. All complaints will be reviewed and investigated promptly and fairly.

## Recognition

Contributors who make significant improvements to RgentAI may be recognized:
- **Listed in CONTRIBUTORS.md**
- **Mentioned in release notes**
- **Invited to be maintainers** for substantial ongoing contributions

## License

By contributing to RgentAI, you agree that your contributions will be licensed under the same MIT License that covers the project. See the LICENSE file for details.

## Thank You!

Thank you for your interest in contributing to RgentAI! Your contributions help make statistical analysis more accessible to researchers worldwide.
