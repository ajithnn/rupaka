# Rupaka

## Format Description 

### Config Format 
- Types : 
  - num :  Numeric type , represented as a double so can take decimal points
  - str :  String type can have spaces 
  - bool:  Boolean data type supports True/False or true/false
  - obj :  Object type begins with { and ends with } and can contain any other type in between
  - [num] : Numerical array , between [ and ]
  - [str] : String array between [ and ]
  - [bool]: Boolean array between [ and ]
  - [obj] : Array of Objects between [ and ]
- Each line starts with one of the above types 
- Opening brackets for obj and [obj] must have only the bracket after the separator and then a newline (refer examples)
- Closing Brackets for obj and [obj] must end on a new line (refer examples)
- Obj type can have any other types within it including obj and [obj] types

### Validation Format
- Types : 
  - num :  Numeric type , represented as a double so can take decimal points
  - str :  String type can have spaces 
  - bool:  Boolean data type supports True/False or true/false
  - [num] : Numerical array , between [ and ]
  - [str] : String array between [ and ]
  - [bool]: Boolean array between [ and ]
  - Can have validations for any of the obove types 
  - For obj types have validation at individual fields of the object using key format "top-key>sub-key>sub-key"
    - ">" is used to separate the hierarchy of keys
  - For [obj] types also use the same format as above and each of the objs in array is validated against

## Example Simple Config 

```
num threads : 10
num users : 3
str usertoken : asdfghjkl
```

## Example Simple Validation 

```
num threads < 15
num users > 1

```


## Example Object Config 

```
obj male : {
str url : https://asdf.com
bool run : True
str mode : none
num age : 32.0234
obj office : {
str desg : Architect
str company : amagi
}
}
obj female : {
str url : https://efgh.com
bool run : True
str mode : none
num age : 31.0234
}
str kid : qwerty
num house : 148
[num] dimensions : [25.0,51.0,25.0,51.0]
```

## Example Object Validation 

```
str kid matches ^[a-z]+$
str male>office>company matches ^[a-z]+$
str female>url matches ^http(s)*:\/\/[a-z]+\.(com|in|net)$
```

## Example Array of Objects Config 

```
obj male : {
str url : https://asdf.com
bool run : True
str mode : none
num age : 32.0234
[obj] offices : [
{
str location : blr
str desg : Architect
str company : amagi
}
{
str location : delhi
str desg : EM
str company : amagi
}
]
}
obj female : {
str url : https://efgh.com
bool run : True
str mode : none
num age : 31.0234
}
str kid : qwerty
num house : 148
[num] dimensions : [25.0,51.0,25.0,51.0]
```

## Example Array of Objects Validation 

```
str kid matches ^[a-z]+$
str male>offices>company matches ^[a-z]+$
str female>url matches ^http(s)*:\/\/[a-z]+\.(com|in|net)$
```

More examples in ```./examples``` folder

## Run Commands

```
./rupaka -o "examples/outputs-5.json" -c "examples/example-5.cfg" -v "examples/example-5.vld"
```

```
./rupaka -o "examples/outputs-4.json" -c "examples/example-4.cfg" -v "examples/example-4.vld"
``` 
