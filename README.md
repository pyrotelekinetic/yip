# yip

A very simple pre processor.

> NB: yip is still in early development, expect breaking changes.


## Terminology
### Template
yip evaluates templates.

Templates contain any combination of:
- literal text
- templates
- fields

> Because literal text is valid in a template, any valid text file is a valid
> template.

### Field
A field is a placeholder for a value. The value is specified in the parent
template, in the form `field:value`.


## Syntax
Templates have a simple syntax to describe their format.

Another template is written as: `{:template.txt field:content:}`, where
`template.txt` is the absolute or relative path to the template.

A field is written as: `{!name!}`, where `name` is the name of a field passed
in the field parameter of the containing template.

A literal is any text that does not match the above syntaxes.


---

    Copyright (C) 2023,2024  Carter "pyrotelekinetic" Ison <carter@isons.org>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
