/*
The MIT License (MIT)

Copyright (c) 2018-2022 Susam Pal

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// TeXMe - Self-rendering Markdown + LaTeX documents

(function () {
  'use strict'

  /**
   * Private namespace of TeXMe. The members of the inner namespace
   * are inaccessible outside TeXMe.
   *
   * @namespace inner
   */

  /**
   * The markdown module is loaded and assigned to this variable.
   *
   * @type object
   * @memberof inner
   */
  let markdown

  /**
   * Exported module of TeXMe.
   *
   * @exports texme
   */
  const texme = {}

  /**
   * Configuration options object. Each configuration option is set as a
   * property of this object.
   *
   * @type object
   * @memberof inner
   */
  const options = {}

  /**
   * Set default configuration options.
   */
  texme.setDefaultOptions = function () {
    options.renderOnLoad = true
    options.useMathJax = true
    options.protectMath = true
    options.style = 'none'
    options.onRenderPage = undefined

    // Update "Configuration Options" section of README.md if any of the
    // following URLs is updated.
    options.markdownURL =
      'https://cdn.jsdelivr.net/npm/marked@4.0.12/marked.min.js'
    options.MathJaxURL =
      'https://cdn.jsdelivr.net/npm/mathjax@3.2.0/es5/tex-mml-chtml.js'
    options.hljsURL =
      'https://cdn.jsdelivr.net/npm/highlightjs@9.16.2/highlight.pack.min.js'
  }

  /**
   * Read configuration options specified in `window.texme` and
   * configure TeXMe.
   *
   * @memberof inner
   */
  const setWindowOptions = function () {
    for (const key in options) {
      if (typeof window !== 'undefined' &&
          typeof window.texme !== 'undefined' &&
          typeof window.texme[key] !== 'undefined') {
        options[key] = window.texme[key]
      }
    }
  }

  /**
   * Set configuration option.
   *
   * @param {string} key - Configuration option name
   * @param {object} val - Configuration value object
   */
  texme.setOption = function (key, val) {
    options[key] = val
  }

  /**
   * Load JS in browser environment.
   *
   * @param {string} url - URL of JavaScript file.
   * @param {function} callback - Callback to invoke after script loads.
   * @memberof inner
   */
  const loadjs = function (url, callback) {
    const script = window.document.createElement('script')
    script.src = url
    script.onload = callback
    window.document.head.appendChild(script)
  }

  /**
   * Enumeration of texme.tokenTypes.
   *
   * @enum {number}
   */
  texme.tokenType = {
    /** Markdown token */
    MARK: 0,

    /** Math token or mask-literal token */
    MASK: 1
  }

  /**
   * Enumeration of special literals. Currently there is only one.
   *
   * @enum {string}
   */
  texme.tokenLiteral = {

    /**
     * Mask literal used to mask math content. All mathematical
     * snippets detected in the content are replaced with this mask
     * literal before performing Markdown rendering on it. This
     * prevents from the Markdown renderer from seeing and
     * processing any math content.
     */
    MASK: '::MASK::'
  }

  /**
   * Tokenize input text containing Markdown and LaTeX code.
   *
   * @param {string} s - Text with Markdown and LaTeX code.
   *
   * @returns {Array<Array<string>>} An array of tokens of the form
   *   `[ [<type>, <value>], [<type>, [value], ...] ]` where each
   *   token of the form `[<type>, <value>]` is an array of two
   *   values: {@link module:texme.texme.tokenType tokenType} and token
   *   value (string).
   */
  texme.tokenize = function (s) {
    const pattern = [
      '\\\\begin{(.*?)}([\\s\\S]*?)\\\\end{\\1}', // \begin{..}..\end{..}
      '\\\\\\[[\\s\\S]*?\\\\\\]', // \[..\]
      '\\\\\\([\\s\\S]*?\\\\\\)', // \(..\)
      '\\\\\\$', // \$ (literal dollar supported by processEscapes)
      '\\$\\$(?:[^\\\\]|\\\\.)*?\\$\\$', // $$..$$
      '\\$(?:[^$\\\\]|\\\\.)+?\\$', // $..$
      texme.tokenLiteral.MASK // ::MASK::
    ].join('|')
    const re = new RegExp(pattern, 'g')

    let result
    let mdText
    const tokens = []
    let nextIndex = 0

    while ((result = re.exec(s)) !== null) {
      // Markdown text
      if (result.index > nextIndex) {
        mdText = s.substring(nextIndex, result.index)
        tokens.push([texme.tokenType.MARK, mdText])
      }

      if (typeof result[1] !== 'undefined' && result[1].startsWith('md')) {
        // Protected code block
        tokens.push([texme.tokenType.MARK, result[2]])
      } else {
        // Masked text (LaTeX or mask-literal)
        tokens.push([texme.tokenType.MASK, result[0]])
      }

      // Start of next Markdown text
      nextIndex = re.lastIndex
    }

    // Trailing Markdown text
    mdText = s.substring(nextIndex)
    if (s.length > nextIndex) {
      tokens.push([texme.tokenType.MARK, mdText])
    }

    return tokens
  }

  /**
   * Construct Markdown text from the specified tokens such that any
   * LaTeX tokens and mask-literal tokens are masked. The returned
   * string is a masked string containing only Markdown text and no
   * LaTeX code at all. All LaTeX code in it is masked with
   * mask-literal. So the returned text can now be used to render the
   * Markdown content in it to HTML without affecting any LaTeX code.
   *
   * @param {Array<Array<string>>} tokens - An array of tokens
   * returned by the {@link module:texme.tokenize tokenize} function.
   *
   * @returns {{text: string, tokenValues: Array<string>}} An object
   * with two properties: `text` with the masked string as its value
   * and `tokenValues` with its value as an array of original tokens
   * that were replaced with masks in the masked string.
   */
  texme.mask = function (tokens) {
    const maskedText = []
    const maskedTokenValues = []
    let tokenType
    let tokenValue
    let i

    for (i = 0; i < tokens.length; i++) {
      tokenType = tokens[i][0]
      tokenValue = tokens[i][1]

      if (tokenType === texme.tokenType.MARK) {
        maskedText.push(tokenValue)
      } else { // if (tokenType === texme.tokenType.MASK)
        maskedText.push(texme.tokenLiteral.MASK)
        maskedTokenValues.push(tokenValue)
      }
    }

    return {
      text: maskedText.join(''),
      tokenValues: maskedTokenValues
    }
  }

  /**
   * Replace mask-literal tokens with the corresponding content.
   *
   * @param {string} s - A string containing mask-literals.
   * @param {Array<string>} tokens - Arary of token values that were
   * masked by the {@link module:texme#mask mask} function. This array
   * is available as the value of `tokenValues` property of the object
   * returned by the {@link module:texme#mask mask} function.
   *
   * @returns {string} Unmasked text with mask-literal tokens replaced
   * with the original token values.
   */
  texme.unmask = function (s, tokens) {
    const re = new RegExp(texme.tokenLiteral.MASK, 'g')
    let i = 0
    return s.replace(re, function () { return tokens[i++] })
  }

  /**
   * Render Markdown content to HTML.
   *
   * @param {string} s - Markdown content.
   *
   * @returns {string} Rendered HTML.
   */
  texme.renderMarkdown = function (s) {
    return markdown(s)
  }

  /**
   * Render Markdown content while ensuring that LaTeX content is not
   * interpreted and rendered as Markdown.
   *
   * @param {string} s - Markdown + LaTeX content.
   *
   * @returns {string} Rendered HTML.
   */
  texme.protectMathAndRenderMarkdown = function (s) {
    const tokens = texme.tokenize(s)
    const masked = texme.mask(tokens)
    const rendered = texme.renderMarkdown(masked.text)
    const unmasked = texme.unmask(rendered, masked.tokenValues)
    return unmasked
  }

  /**
   * Render Markdown and/or LaTeX content into HTML.
   *
   * If the configuration option `protectMath` is `true` (the default),
   * then LaTeX content is protected from Markdown renderer. Otherwise,
   * the entire content is rendered as Markdown.
   *
   * @param {string} s - Markdown + LaTeX content.
   */
  texme.render = function (s) {
    if (options.protectMath) {
      return texme.protectMathAndRenderMarkdown(s)
    } else {
      return texme.renderMarkdown(s)
    }
  }

  /**
   * Set page to display the rendered content as HTML.
   */
  texme.renderPage = function () {
    let md
    const elements = window.document.getElementsByClassName('texme')
    for (let element of elements) {
      md = texme.render(element.value.trim())
      element.outerHTML = md
    }

    var aCodes = document.getElementsByTagName('pre');
    for (let code of aCodes) {
      hljs.highlightBlock(code);
    }

    // Typeset LaTeX.
    if (options.useMathJax) {
      window.MathJax.typesetPromise()
    }

    // Invoke onRenderPage callback (if configured).
    if (typeof options.onRenderPage !== 'undefined') {
      options.onRenderPage()
    }
  }

  /**
   * Set up dependencies and set page.
   */
  texme.main = function () {
    texme.setDefaultOptions()

    if (typeof window !== 'undefined') {
      setWindowOptions()

      loadjs(options.hljsURL)

      loadjs(options.markdownURL, function () {
        window.marked.setOptions({
          highlight: function(code, lang) {
            return hljs.highlight(lang, code).value;
          }
        });
        markdown = window.marked.parse
      })

      if (options.useMathJax) {
        // MathJax configuration.
        window.MathJax = {
          tex: {
            // Enable $...$ as delimiter for inline math.
            inlineMath: [['$', '$'], ['\\(', '\\)']],
            tags: 'ams'
          },
          startup: {
            typeset: false
          }
        }

        loadjs(options.MathJaxURL)
      }

      if (options.renderOnLoad) {
        // Render Markdown + LaTeX after the document loads.
        window.onload = texme.renderPage
      }

      window.texme = texme
    } else {
      md = require('marked')
      markdown = md.parse
      module.exports = texme
    }
  }

  texme.main()
})()
