<!DOCTYPE html>
<html lang="" xml:lang="">
  <head>
    <title>Using GitHub for Collaboration</title>
    <meta charset="utf-8" />
    <meta name="author" content="Hao Ye" />
    <meta name="date" content="2021-01-28" />
    <link rel="stylesheet" href="xaringan-themer.css" type="text/css" />
  </head>
  <body>
    <textarea id="source">




class: inverse, center, middle

# Using GitHub for Collaboration
### Hao Ye
### Health Science Center Libraries, University of Florida
### (updated: 2021-01-28)

---
# Motivations

* One of the advantages for using version control is managing collaborative work.
* Git is a flexible tool that accommodates many different styles of collaboration... not all of these styles are suitable for small research teams.
* "GitHub flow" is a workflow that is lightweight and scales well for individuals and small teams.

---
# Learning Outcomes

* describe how merge conflicts originate and how to resolve them
* apply the "GitHub flow" workflow for collaborating
* create and navigate branches on GitHub and using GitHub Desktop
* create and merge pull requests
* understand "forks" and open-source-community development on GitHub

---
# Prerequisites

This lesson assumes you:
* have some familiarity making *repos* and *commits*
* know how to synchronize repos from your computer to GitHub

---
class: inverse, center, middle

# Merge Conflicts

---
# Inviting Collaborators

* I have a github repo, and would like to give my collaborator access to work on the project.
* From the github repo page:
  - `Settings` -&gt; `Manage access`
  - `Invite a collaborator`

---
&lt;img src="github-manage-access.png" title="A screenshot of the website https://github.com/ha0ye/portalDS/settings/access, showing that the repo is visible to the public, and that only the owner can contribute to the repository. There is a green button with the text 'Invite a collaborator'." alt="A screenshot of the website https://github.com/ha0ye/portalDS/settings/access, showing that the repo is visible to the public, and that only the owner can contribute to the repository. There is a green button with the text 'Invite a collaborator'."  /&gt;

---
# A Simple Workflow

Two people, A and B, are working together on one repo in GitHub:
* A makes commits, A pushes the new commits to GitHub.
* B pulls the latest commits from A, works on new commits, and pushes their new commits back to GitHub.
* A pulls the latest commits from B, works on new commits, etc.

---
# What happens if you forget to pull?

You get a warning when you try to push to GitHub.

&lt;img src="newer-commits-warning.png" title="A screenshot of the warning from using GitHub Desktop when trying to push new commits to GitHub, and there are commits on GitHub that have not yet been fetched and merged locally. The warning reads 'Newer Commits on Remote; Desktop is unable to push commits to this branch because there are commits on the remote that are not present on your local branch. Fetch these new commits before pushing in order to reconcile them with your local commits.'" alt="A screenshot of the warning from using GitHub Desktop when trying to push new commits to GitHub, and there are commits on GitHub that have not yet been fetched and merged locally. The warning reads 'Newer Commits on Remote; Desktop is unable to push commits to this branch because there are commits on the remote that are not present on your local branch. Fetch these new commits before pushing in order to reconcile them with your local commits.'"  /&gt;

---
# Merging

* a `git merge` combine the changes from diverging commits:
  - suppose A and B start with commit: `{orig}`
  - A makes a commit, `{a}`, with changes
  - B makes a commit, `{b}`, with changes
  - `git merge` combines `{a}` and `{b}` into a new commit `{c}`, that contains both sets of changes.
  
---
# Merging (automatically)

* if the changes are in different files

  OR different parts of the same files
  - git is generally able to combine the changes without further intervention

* if git is unable to merge automatically, then it is a `merge conflict`

---
# Resolving merge conflicts

* when git cannot combine the changes automatically, you must manually create the merged file:
&lt;img src="merge-conflict.png" title="A screenshot of BBedit showing the merge conflict. Line 6 is '&amp;lt;&amp;lt;&amp;lt;&amp;lt;&amp;lt;&amp;lt;&amp;lt; HEAD'`' indicating the start of one version of the file. Line 8 is '=======' indicating the end of one version and the beginning of the next. Line 10 shows '&amp;gt;&amp;gt;&amp;gt;&amp;gt;&amp;gt;&amp;gt;&amp;gt;' followed by a hash, indicating the end of the second version." alt="A screenshot of BBedit showing the merge conflict. Line 6 is '&amp;lt;&amp;lt;&amp;lt;&amp;lt;&amp;lt;&amp;lt;&amp;lt; HEAD'`' indicating the start of one version of the file. Line 8 is '=======' indicating the end of one version and the beginning of the next. Line 10 shows '&amp;gt;&amp;gt;&amp;gt;&amp;gt;&amp;gt;&amp;gt;&amp;gt;' followed by a hash, indicating the end of the second version."  /&gt;

---
# Resolving merge conflicts 2

* After the conflicts are resolved, you need to create a new commit with the merged edits.
* This merged commit can then be pushed to github without issue.
* For non-text files, you may need to revert back to one or the other version (whichever is correct)

---
# Summary

* This approach works pretty well when there is one primary contributor, and collaborators rarely make changes.
  - otherwise everyone needs to be constantly pulling and merging, contacting each other through separate channels to let them know you are done pushing changes to GitHub, etc.

---
class: inverse, center, middle

# GitHub Flow

---
# Essentials of GitHub Flow

Manage work using branches - `merge` branches back to the primary branch when ready.

&lt;img src="github-flow.png" title="A screenshot of the diagram from https://guides.github.com/introduction/flow/, showing conceptually how the 'GitHub flow' workflow works." alt="A screenshot of the diagram from https://guides.github.com/introduction/flow/, showing conceptually how the 'GitHub flow' workflow works."  /&gt;

---
# Steps

1. Create a branch and switch to it.
2. Add commits with desired changes.
3. Open a pull request (PR).
4. Review the PR if necessary, and merge.
5. Delete branch if done.

---
class: inverse, center, middle

# Navigating Branches

---
# What is a Branch?

* A `branch` is a label for a set of commits.

&lt;img src="git-branch.svg" title="A diagram from https://www.atlassian.com/git/tutorials/using-branches, of a git repo represented in a diagram with commits as circular nodes, with edges between them showing the relationship between commits and their parents. There is a `Master` branch in light blue, a `Little Feature` branch in purple with one commit, and a `Big Feature` branch in green with 3 commits." alt="A diagram from https://www.atlassian.com/git/tutorials/using-branches, of a git repo represented in a diagram with commits as circular nodes, with edges between them showing the relationship between commits and their parents. There is a `Master` branch in light blue, a `Little Feature` branch in purple with one commit, and a `Big Feature` branch in green with 3 commits." width="80%" /&gt;
.small[[img: https://www.atlassian.com/git/tutorials/using-branches]]

---
# How do branches work?

* By default, you are in the primary branch.
  - each commit has changes from the previous commit
  - a linear sequence of versions of the project
* When you want to make changes and commits without disrupting the primary branch:
  - create a new branch
  - merge when ready

---
# Creating new branches (GitHub)

&lt;img src="github-new-branch.png" title="screenshot of the GitHub interface, when clicking on the pulldown menu to switch branches; this interface also enables one to create new branches." alt="screenshot of the GitHub interface, when clicking on the pulldown menu to switch branches; this interface also enables one to create new branches."  /&gt;

---
class: center, middle

# DEMO

---
# Creating new branches (GitHub Desktop)

![screenshot of the GitHub Desktop interface, when clicking on the pulldown menu to switch branches; this interface also enables one to create new branches.](github-desktop-new-branch.png)

---
class: center, middle

# DEMO

---
class: inverse, center, middle

# Merging Branches

---
# Merging (branches)

* Nearly the same as merging commits:
  - merge the last commit on another branch to the current branch.

1. switch to the branch that will keep the merged result
2. merge from the other branch

---
class: center, middle

# DEMO (GitHub Desktop)

---
# Pull Request (GitHub)

* recommended practice for merges on GitHub
* lets you document the merge
  - can request official "review"
  - other collaborators can comment, make further changes

---
class: center, middle

# DEMO (GitHub)

---
class: inverse, center, middle

# Community-oriented Development on GitHub

---
# Issues

* reported bugs, feature requests, etc.
* anyone can contribute to discussion
* can be referred to by number in commits and pull requests

---
class: center, middle

# DEMO

---
# Forks

* How do you contribute to other people's projects?
  - Fork = clone a repo on GitHub
  - Forks retain memory of original repo (and cannot have their own issues)
  - Pull Requests can merge from across forks.

* "Fork and PR" is a common phrase indicating how one should contribute to an open-source project.

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
