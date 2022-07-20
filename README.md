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


### Supported Validations

- String Validations
  - str key matches ^regex$ : Validates if the value against the key matches the given regex (Regex support is limited : TODO : allow full fledged regex)
  - str key>subkey>subkey matches ^regex$ : Validates if the value against the key.subkey.subkey matches regex (traverses objects)
  - [str] key matches ^regex$ : Validates each value in the array of the key matches against the regex 
  - str key not_matches ^regex$ : Ensures the value against the key does not matches the given regex (Regex support is limited : TODO : allow full fledged regex)
  - str key>subkey>subkey not_matches ^regex$ : Ensures the value against the key.subkey.subkey does not matches regex (traverses objects)
  - [str] key not_matches ^regex$ : Ensures each value in the array of the key does not matches against the regex 
  - str key oneof enum1|enum2|enum3|enum4 : Ensures the value against the key is one the given enum values
  - str key>subkey>subkey oneof enum1|enum2 : Ensures the value against the key.subkey.subkey is one of the given enum values(traverses objects)
  - [str] key not_matches enum1|enum2|enum3|enum4 : Ensures each value in the array of the key is one of the given enum values 
  - str key not_oneof enum1|enum2|enum3|enum4 : Ensures the value against the key is one the given enum values
  - str key>subkey>subkey not_oneof enum1|enum2|enum3|enum4 : Ensures the value against the key.subkey.subkey is one of the given enum values(traverses objects)
  - [str] key not_oneof enum1|enum2|enum3|enum4 : Ensures each value in the array of the key is one of the given enum values 
  - str key length_gt number : Validates the length of the string value is greater than the given number
    - Similarly length_gte,length_lt,length_lte,length_eq are also supported
    - The above also works for sub-keys and arrays of strings as well
- Numeric Validations 
  - num key ( > |< | <= | >= | == ) value : Compares the key to value with the given operator
    - Can use multiple conditions in separate lines to provide between conditions eg: < 20 , > 10 etc
    - The same is supported
    - for array of numbers and numbers inside object sub-keys
- TODO Validations 
  - Key validations : Add allowed key validations at top level and sub-key levels
    - key k oneof allowedkeys 
    - key k not_oneof allowedkeys
  - Required Validation : Add required key at the top and sub key levels  

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
[num] dimensions : [25.0, 51.0, 25.0, 51.0]
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
[num] dimensions : [25.0, 51.0, 25.0, 51.0]
```

## Example Array of Objects Validation 

```
str kid matches ^[a-z]+$
str male>offices>company matches ^[a-z]+$
str female>url matches ^http(s)*:\/\/[a-z]+\.(com|in|net)$
```

More examples in ```./examples``` folder

## Run Commands on Linux

```
./rupaka -o "examples/outputs-5.json" -c "examples/example-5.cfg" -v "examples/example-5.vld"
```

```
./rupaka -o "examples/outputs-4.json" -c "examples/example-4.cfg" -v "examples/example-4.vld"
``` 


### Output 

```
cat examples/outputs-4.json | jq -r .
cat examples/outputs-5.json | jq -r .
```

## Run Commands with docker (for macos)

```
docker pull slashring/rupaka:v0.0.5
```

```
docker run --rm -i --mount type=bind,src="$(pwd)"/examples,dst=/app/rupaka/examples -t slashring/rupaka:v0.0.5 -o /app/rupaka/examples/output-d.json -c /app/rupaka/examples/example-1.cfg -v /app/rupaka/examples/example-1.vld
```

### Output 

```
cat examples/output-d.json | jq -r .
```

## Roadmap 

1. Add more validations at individual fields level
2. Add validations for array and array of objects like length of array etc
3. Add object validations like allowed keys / required keys 
4. Add required keys validation at top level


