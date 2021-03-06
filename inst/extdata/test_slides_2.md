<!DOCTYPE html>
<html lang="" xml:lang="">
  <head>
    <title>Converts Xaringan Slides Into Markdown Notes</title>
    <meta charset="utf-8" />
    <meta name="author" content="Hao Ye" />
    <meta name="date" content="2021-01-28" />
    <link rel="stylesheet" href="xaringan-themer.css" type="text/css" />
  </head>
  <body>
    <textarea id="source">




class: inverse, center, middle

# Converts Xaringan Slides Into Markdown Notes
### Hao Ye
### Health Science Center Libraries, University of Florida
### (updated: 2021-01-28)

---
class: inverse, center, middle

## "Any fool can write code that a computer can understand. &lt;br /&gt;Good programmers write code that humans can understand."

&lt;p style="text-align:right"&gt;from "Refactoring: Improving the Design of Existing Code" by Martin Fowler&lt;/p&gt;

---
# Motivations

* In addition to code that works, ideally it is **also**:
  - easy to read and understand
  - easy to maintain or change
  - obvious in its correctness
  - aesthetically pleasing?

---
# Learning Outcomes

By the end of the workshop, participants will be able to:

* implement functions to make code more modular and increase reproducibility
* use data structures to manage inputs and outputs
* recognize and fix basic code smells

---
# Disclaimer

* Concepts are intended to be universal, but code examples will be in **`R`**.
* Like any other skill, *effective* practice matters!
  - working code != easy-to-read code (e.g. answers from Stack Overflow)
  - when reading, ask yourself "is it clear what is going on?" (even if you don't know the detailed mechanisms)

---
class: inverse, center, middle

# Breaking Code into Functions

---
# What are functions?

* **Functions** let you refer to another piece of code by (a hopefully informative!) name


```r
  mean()
  # computes arithmetic mean of input
```

* You can write your own functions, too!

```r
  celsius_to_fahrenheit &lt;- function(x) {
    9/5 * x + 32
  }
```

---
# Why write your own functions?

* It seems like extra work...

--
* BUT, it enables:
  - repeating the same operation on different inputs (e.g. datasets, variables, parameter values, etc.)
  - clarifying the larger organizational structure of the code

---
# Duplication


```r
df &lt;- data.frame( a = rnorm(10),
                  b = rnorm(10),
                  c = rnorm(10))

# rescale all the columns of df
df$a &lt;- (df$a - min(df$a)) / 
  (max(df$a) - min(df$a))
df$b &lt;- (df$b - min(df$b)) / 
  (max(df$b) - min(df$a))
df$c &lt;- (df$c - min(df$c)) / 
  (max(df$c) - min(df$c))
```

---
# Define a function!


```r
rescale01 &lt;- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# rescale all the columns of df
df$a &lt;- rescale01(df$a)
df$b &lt;- rescale01(df$b)
df$c &lt;- rescale01(df$c)

# or with dplyr
df &lt;- df %&gt;% mutate_at(c("a", "b", "c"), 
                       rescale01)
```

---
# Notes

* calculation defined once and re-used
* changes or corrections only need to happen in one place
* fewer chances for error when modifying after copy-paste

---
# Workflow structure

&lt;img src="project-diagram.svg" title="Diagram of the workflow in a hypothetical data analysis project with boxes representing code and data/output files. &amp;quot;Raw data&amp;quot; goes into &amp;quot;Pre-processing&amp;quot; and then &amp;quot;Pre-processed data&amp;quot;. &amp;quot;Pre-processed data&amp;quot; goes directly into &amp;quot;Figure 3&amp;quot; (code) and then &amp;quot;Figure 3&amp;quot; (Data file), but also into &amp;quot;Analysis/Modelling&amp;quot;. The &amp;quot;Model&amp;quot; output from &amp;quot;Analysis/Modelling&amp;quot; is used in code for &amp;quot;Figure 1&amp;quot; and &amp;quot;Figure 2&amp;quot;, which generate files &amp;quot;Figure 1&amp;quot; and &amp;quot;Figure 2&amp;quot;." alt="Diagram of the workflow in a hypothetical data analysis project with boxes representing code and data/output files. &amp;quot;Raw data&amp;quot; goes into &amp;quot;Pre-processing&amp;quot; and then &amp;quot;Pre-processed data&amp;quot;. &amp;quot;Pre-processed data&amp;quot; goes directly into &amp;quot;Figure 3&amp;quot; (code) and then &amp;quot;Figure 3&amp;quot; (Data file), but also into &amp;quot;Analysis/Modelling&amp;quot;. The &amp;quot;Model&amp;quot; output from &amp;quot;Analysis/Modelling&amp;quot; is used in code for &amp;quot;Figure 1&amp;quot; and &amp;quot;Figure 2&amp;quot;, which generate files &amp;quot;Figure 1&amp;quot; and &amp;quot;Figure 2&amp;quot;." width="60%" /&gt;

.tiny[modified from "Reproducible research best practices @JupyterCon" (version 18) by Rachael Tatman, https://www.kaggle.com/rtatman/reproducible-research-best-practices-jupytercon]

---
# Example Code


```r
data_raw &lt;- readRDS("readings.dat")
data_proc &lt;- preprocess_data(data_raw)

fitted_model &lt;- run_model(data_proc)

plot_model_forecasts(fitted_model,
    "figure-1_abundance-forecasts.pdf")
plot_abundance_residuals(fitted_model,
    "figure-2_abundance-residuals.pdf")
plot_abundance_histogram(data_proc, 
    "figure-3_abundance-residuals.pdf")
```

---
# Notes

* The steps of the analysis are clear.
* When making changes/additions:
  - you know which function to change
  - adding a new analysis: write a new function, include it in the workflow script
* Ideally save `data_proc` and `fitted_model` objects to a file. Then figure code can be changed without re-running the model.

---
# Tips for writing functions

* name things well
* plan for flexibility
* split large tasks into smaller units

---
# Tip 1: Naming Things

* function names should be verbs


```r
# bad
row_adder()
permutation()

# good
add_row()
permute()
```

.small[examples from https://style.tidyverse.org/functions.html#naming]
---
# Tip 2: Plan for flexibility


```r
plot_abundance_histogram &lt;- 
  function(data_proc, filename, 
           width = 6, height = 6) { 
    # {{code}} 
  }
```
* input data and location of output are easily changed
* width and height are adjustable, but have reasonable defaults

---
# Tip 3: Function size

* Each function should have a single well-defined task
  - this makes testing and debugging easier
* Functions should ideally be 50 lines or less
  - not a hard rule, divide work into functions sensibly!

---
# Tip 3 (cont'd)

* If a line or set of lines of code is complicated, it might need to be its own function (with a good name)


```r
# bad
if (class(x)[[1]]) == "numeric" || 
    class(x)[[1]] == "integer")

# good
if (is.numeric(x))
```
.small[examples from https://speakerdeck.com/jennybc/code-smells-and-feels?slide=36]
---
class: inverse, center, middle

# Code Smells

---
# What are "code smells"

* aspects of the code that make it appear less ideal
* the code is not necessarily buggy  
--
  - but it is hard to tell!

---
# Example (code smell)



---
# Solution

* follow a coding style guide
* use technological tools like automatic indentation and linters
* understand patterns and how to fix them
  - **refactoring**: rewriting code without changing its behavior (i.e. make it faster, cleaner, easier to use)

---

* indentation
* spaces


---
# Comments



---
# Thanks

* Let me know what content you'd like to see
* Contact me for additional questions or consultation requests!
* Check back in on the libguide for more modules and contact info:
  - https://guides.uflib.ufl.edu/reproducibility
    </textarea>
<style data-target="print-only">@media screen {.remark-slide-container{display:block;}.remark-slide-scaler{box-shadow:none;}}</style>
<script src="https://remarkjs.com/downloads/remark-latest.min.js"></script>
<script>var slideshow = remark.create({
"highlightStyle": "github",
"highlightLines": true,
"countIncrementalSlides": false
});
if (window.HTMLWidgets) slideshow.on('afterShowSlide', function (slide) {
  window.dispatchEvent(new Event('resize'));
});
(function(d) {
  var s = d.createElement("style"), r = d.querySelector(".remark-slide-scaler");
  if (!r) return;
  s.type = "text/css"; s.innerHTML = "@page {size: " + r.style.width + " " + r.style.height +"; }";
  d.head.appendChild(s);
})(document);

(function(d) {
  var el = d.getElementsByClassName("remark-slides-area");
  if (!el) return;
  var slide, slides = slideshow.getSlides(), els = el[0].children;
  for (var i = 1; i < slides.length; i++) {
    slide = slides[i];
    if (slide.properties.continued === "true" || slide.properties.count === "false") {
      els[i - 1].className += ' has-continuation';
    }
  }
  var s = d.createElement("style");
  s.type = "text/css"; s.innerHTML = "@media print { .has-continuation { display: none; } }";
  d.head.appendChild(s);
})(document);
// delete the temporary CSS (for displaying all slides initially) when the user
// starts to view slides
(function() {
  var deleted = false;
  slideshow.on('beforeShowSlide', function(slide) {
    if (deleted) return;
    var sheets = document.styleSheets, node;
    for (var i = 0; i < sheets.length; i++) {
      node = sheets[i].ownerNode;
      if (node.dataset["target"] !== "print-only") continue;
      node.parentNode.removeChild(node);
    }
    deleted = true;
  });
})();
(function() {
  "use strict"
  // Replace <script> tags in slides area to make them executable
  var scripts = document.querySelectorAll(
    '.remark-slides-area .remark-slide-container script'
  );
  if (!scripts.length) return;
  for (var i = 0; i < scripts.length; i++) {
    var s = document.createElement('script');
    var code = document.createTextNode(scripts[i].textContent);
    s.appendChild(code);
    var scriptAttrs = scripts[i].attributes;
    for (var j = 0; j < scriptAttrs.length; j++) {
      s.setAttribute(scriptAttrs[j].name, scriptAttrs[j].value);
    }
    scripts[i].parentElement.replaceChild(s, scripts[i]);
  }
})();
(function() {
  var links = document.getElementsByTagName('a');
  for (var i = 0; i < links.length; i++) {
    if (/^(https?:)?\/\//.test(links[i].getAttribute('href'))) {
      links[i].target = '_blank';
    }
  }
})();
// adds .remark-code-has-line-highlighted class to <pre> parent elements
// of code chunks containing highlighted lines with class .remark-code-line-highlighted
(function(d) {
  const hlines = d.querySelectorAll('.remark-code-line-highlighted');
  const preParents = [];
  const findPreParent = function(line, p = 0) {
    if (p > 1) return null; // traverse up no further than grandparent
    const el = line.parentElement;
    return el.tagName === "PRE" ? el : findPreParent(el, ++p);
  };

  for (let line of hlines) {
    let pre = findPreParent(line);
    if (pre && !preParents.includes(pre)) preParents.push(pre);
  }
  preParents.forEach(p => p.classList.add("remark-code-has-line-highlighted"));
})(document);</script>

<script>
slideshow._releaseMath = function(el) {
  var i, text, code, codes = el.getElementsByTagName('code');
  for (i = 0; i < codes.length;) {
    code = codes[i];
    if (code.parentNode.tagName !== 'PRE' && code.childElementCount === 0) {
      text = code.textContent;
      if (/^\\\((.|\s)+\\\)$/.test(text) || /^\\\[(.|\s)+\\\]$/.test(text) ||
          /^\$\$(.|\s)+\$\$$/.test(text) ||
          /^\\begin\{([^}]+)\}(.|\s)+\\end\{[^}]+\}$/.test(text)) {
        code.outerHTML = code.innerHTML;  // remove <code></code>
        continue;
      }
    }
    i++;
  }
};
slideshow._releaseMath(document);
</script>
<!-- dynamically load mathjax for compatibility with self-contained -->
<script>
(function () {
  var script = document.createElement('script');
  script.type = 'text/javascript';
  script.src  = 'https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-MML-AM_CHTML';
  if (location.protocol !== 'file:' && /^https?:/.test(script.src))
    script.src  = script.src.replace(/^https?:/, '');
  document.getElementsByTagName('head')[0].appendChild(script);
})();
</script>
  </body>
</html>
