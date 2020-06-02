# pathviewR
Insert description here


## Guidelines for writing code
These are informed by [the rOpenSci guide](https://devguide.ropensci.org/) 
as well as my personal opinion
- Always spell out `TRUE` and `FALSE` entirely, e.g. use `argument = TRUE` 
instead of `argument = T`
- Limit line lengths to no more than 80 characters
	- In RStudio, I recommend going to Tools -> Global Options -> Code -> 
  Display, then check the box next to `Show margin` and set `Margin 
  column` to 80. This will generate a vertical guide line to point out 
  where 80 characters is in your scripts.
- Use snake_case for naming all functions, arguments, and variables 
	- Try to go with `object_verb()` or `verb_object()` naming schemes for 
  functions when possible, e.g. `stri_join()` or `read_csv()`
	- Try to avoid using function names that appear in other packages, esp 
  those in popular packages like `ggplot2` or `dplyr`, e.g. don't make a 
  `read_csv()`
- Anywhere you rely on a function from another package, you must use 
explicit naming in a `package_name::function()` format, e.g. 
`readxl::read_excel()`
- Avoid acronyms whenever possible unless something is more commonly known 
by its acronym (e.g. "csv")
- For functions, make the object/data the first argument whenever possible 
to allow it to be pipeable (`%>%`)
- rOpenSci advises against using `<-` for assignment and prefers `=` 
instead. That said, it's not a big deal so long as you are generally 
consistent with your choice
- `spelling::spell_check_package()` is your best friend
