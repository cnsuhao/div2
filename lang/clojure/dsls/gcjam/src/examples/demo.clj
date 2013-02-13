;; current idea for format

#_(

in this case there are a couple of syntactic nicities:
   "(...)": standard line, takes 1 line off the top of the string, splits it by white space, and parses according to spec
          otherwise it builds a regular expression from the given phrase
   
   "key => stmts": each line takes this form and starts to build up a parser.  each statement can either be a "default line"
          or a custom one using.  Most statements will take advantage of the default line except when parsing an individual
          units of a line.

   "key[.stored-name][!repeat]": each parsed bit contains.  There are included keys from the "default" grammar such as
          primatives.  A grammar can also be extended with :extend grammar.  Adding stored-name changes the default name
          in the which gets created for the type.  Adding a repeat means it will return a list of the parsed instead the
          value.  Variables from previously parsed information is allowed in this context.  
   
   schema are definfed starting with keywords.  This is just 
      


(defgrammar q2008b
   :file => :num-cases :case!num-cases
   :num-cases => (int.v) :as v
   :case => (int.num-lines)
            (int.NA int.NB)
            timetables!num-lines
   :timetables => (datetime.dep datetime.arr)
   :datetime   => int.hour ":" int.min :as [ hour min ])

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(create-struct :num-lines :NA :NB :timetables)
(create-struct :dep :arr)
(defn main
  [{num-lines :num-lines NA :NA NB :NB timetables :timetables}] 
  ...
)

;; python samples
#_(          
replacement_field ::=  "{" [field_name] ["!" conversion] [":" format_spec] "}"
field_name        ::=  arg_name ("." attribute_name | "[" element_index "]")*
arg_name          ::=  [identifier | integer]
attribute_name    ::=  identifier
element_index     ::=  integer | index_string
index_string      ::=  <any source character except "]"> +
conversion        ::=  "r" | "s" | "a"
format_spec       ::=  <described in the next section>


format_spec ::=  [[fill]align][sign][#][0][width][,][.precision][type]
fill        ::=  <a character other than '}'>
align       ::=  "<" | ">" | "=" | "^"
sign        ::=  "+" | "-" | " "
width       ::=  integer
precision   ::=  integer
type        ::=  "b" | "c" | "d" | "e" | "E" | "f" | "F" | "g" | "G" | "n" | "o" | "s" | "x" | "X" | "%"
)