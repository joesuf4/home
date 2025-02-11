#!/usr/bin/env node
const {marked} = require("marked");
const markedMermaid = require("marked-mermaid");
const process = require("process");

var data
marked.use(markedMermaid())

process.stdin.on('data', (chunk) => {
    if (data)
        data += chunk
    else
        data = chunk
})
process.stdin.on('end', () => {
    data = data.toString() + `
<script>
  MathJax = {
    loader: {
      load: [
        'input/tex-base', '[tex]/newcommand', '[tex]/action',
        'output/chtml'
      ]
    },
    tex: {
      inlineMath: [['$', '$'], ['\\(', '\\)']],
      packages: ['base', 'newcommand', 'action']
    }
  };
  </script>
  <script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/startup.js"></script>
  <script src="https://cdn.jsdelivr.net/npm/mermaid@10.7.0/dist/mermaid.min.js"></script>
  <script src="https://www.sunstarsys.com/editor.md/lib/codemirror/codemirror.min.js"></script>
  <script src="https://www.sunstarsys.com/editor.md/lib/codemirror/addons.min.js"></script>
  <script src="https://www.sunstarsys.com/editor.md/lib/codemirror/modes.min.js"></script>
  <script blocking="render" type="text/javascript" async>
    var idx = 0;
    async function render () {
      for (const e of $("body").find(".mermaid").toArray()) {
        const {svg} = await mermaid.render("mermaid-" + ++idx, $(e).text());
        e.outerHTML = "<center>" + svg + "</center>";
      }

      colorize = function(collection, defaultMode) {
        if (!collection) collection = document.body.getElementsByTagName("code");
        for (var i = 0; i < collection.length; ++i) {
          var node = collection[i];
          var mode = node.className.replace("language-","");
          if (mode == "c") mode="text/x-csrc";
          if ($(node).hasClass("cm-s-solarized") || !mode) continue;
          var text =$(node).text();
          node.innerHTML = ""
          CodeMirror.runMode(text, mode, node);
          node.className += " cm-s-solarized";
        }
      }
      colorize();
    }
    render();
  </script>
`
    data = marked.parse(data,{gfm:true})
    process.stdout.write(data)
})
