# R CMD check results


## v0.7.1.1000

```
0 errors | 0 warnings | 1 note
```

+ This is a new release.

Additional comments by reviewer: 

+ **Solved** Please provide a link to the used webservices (e.g.: DGE, IRAG
network,...) to the description field
of your DESCRIPTION file in the form
<http:...> or <https:...>
with angle brackets for auto-linking and no space after 'http:' and
'https:'. 

+ **Solved** Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means.
(If a function does not return a value, please document that too, e.g.
`\value{No return value, called for side effects} or similar)`
Missing Rd-tags:
      `update_covidmx.Rd: \value`

+ **Solved** Some code lines in examples are commented out.
Please never do that. Ideally find toy examples that can be regularly
executed and checked. Lengthy examples (> 5 sec), can be wrapped in
`\donttest{}`.
Examples in comments in:
       `descarga_db.Rd`
`\dontrun{}` should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in `\dontrun{}` adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary. **RESPONSE: The only one is `update_covidmx` which updates the package from Github.**
Please replace `\dontrun` with `\donttest`.
Please unwrap the examples if they are executable in < 5 sec, or replace
`\dontrun{}` with `\donttest{}`.

+ **Solved** You are setting `options(warn=-1)` in your function. This is not allowed. Please rather use `suppressWarnings()` if really needed.

+ **Solved** Please make sure that you do not change the user's options, par or
working directory. If you really have to do so within functions, please
ensure with an *immediate* call of `on.exit()` that the settings are reset
when the function is exited. e.g.:
```
old <- options() # code line i
on.exit(options(old)) # code line i + 1
```
```
options(readr.show_progress = !quiet) # somewhere after
```
e.g.: `-> R/descarga_datos_abiertos_aux.R`
If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.

**OUR RESPONSE**:

```
We've made all the changes suggested by reviewer Beni Altmann:

1) We've eliminated the changes in the user's `options`. 
2) We've added return `\value` to all `.Rd` files. 
3) Added links <> to webservices used in description.
4) We've changed all long (>5 sec) `\dontrun` examples to `donttest` except for one in `update_covidmx`. This function installs an updated version of the package (if available) from Github <https://github.com/RodrigoZepeda/covidmx>. The rationale for this is that the webservices used don't depend on us and weblinks and dataset structures have changed through time. Sometimes changes have occurred twice in the same week! So in order for us to rapidly provide the users with an updated version of the package whenever something from these services changes we set up the `update_covidmx` function (also to keep with CRAN's policies and respect your time by not uploading a package too continuously). 

Thank you for your time and review :)
```
