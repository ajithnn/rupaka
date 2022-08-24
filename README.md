# Rupaka

## Format Description 

### Config Format 
- Types are inferred based on the values provided. 
- Each Line can be one of Number,Boolean,String,Array,Object types


### Validation Format
- Types are inferred based on the conditions and values given.
- Each Line can add a validation for String,Number or Key type of Validation.

### Supported Validations

- String Validations
  - key matches ^regex$ : Validates if the value against the key matches the given regex (Regex support is limited : TODO : allow full fledged regex)
  - key>subkey>subkey matches ^regex$ : Validates if the value against the key.subkey.subkey matches regex (traverses objects)
  - key matches ^regex$ : Validates each value in the array of the key matches against the regex 
  - key not_matches ^regex$ : Ensures the value against the key does not matches the given regex (Regex support is limited : TODO : allow full fledged regex)
  - key>subkey>subkey not_matches ^regex$ : Ensures the value against the key.subkey.subkey does not matches regex (traverses objects)
  - key not_matches ^regex$ : Ensures each value in the array of the key does not matches against the regex 
  - key oneof enum1|enum2|enum3|enum4 : Ensures the value against the key is one the given enum values
  - key>subkey>subkey oneof enum1|enum2 : Ensures the value against the key.subkey.subkey is one of the given enum values(traverses objects)
  - key not_matches enum1|enum2|enum3|enum4 : Ensures each value in the array of the key is one of the given enum values 
  - key not_oneof enum1|enum2|enum3|enum4 : Ensures the value against the key is one the given enum values
  - key>subkey>subkey not_oneof enum1|enum2|enum3|enum4 : Ensures the value against the key.subkey.subkey is one of the given enum values(traverses objects)
  - key not_oneof enum1|enum2|enum3|enum4 : Ensures each value in the array of the key is one of the given enum values 
  - key length_gt number : Validates the length of the string value is greater than the given number
    - Similarly length_gte,length_lt,length_lte,length_eq are also supported
    - The above also works for sub-keys and arrays of strings as well
- Numeric Validations 
  - key ( > |< | <= | >= | == ) value : Compares the key to value with the given operator
    - Can use multiple conditions in separate lines to provide between conditions eg: < 20 , > 10 etc
    - The same is supported
    - for array of numbers and numbers inside object sub-keys

- Key validations : allowed key validations at top level and sub-key levels
  - k allowed [allowedkey1,allowedKey2,allowedKey3]
    - In the above expression k can also be a subkey path eg: top-key>sub-key>sub-key
    - For top level keys use CONFIGROOTKEY in place of k
- Required Validation : required key at the top and sub key levels  
  - k required [reqdkey1,reqdKey2,reqdKey3]
    - In the above expression k can also be a subkey path eg: top-key>sub-key>sub-key
    - For top level keys use CONFIGROOTKEY in place of k


## Example Simple Config 

```
threads : 10
users : 3
usertoken : asdfghjkl
```

## Example Simple Validation 

```
threads < 15
users > 1

```


## Example Object Config 

```
male : {
    url : https://asdf.com
    run : True
    mode : none
    age : 32.0234
    office : {
        desg : Architect
        company : amagi
    }
}

female : {
    url : https://efgh.com
    run : True
    mode : none
    age : 31.0234
}

kid : qwerty
house : 148
dimensions : [25.0, 51.0, 25.0, 51.0]
```

## Example Object Validation 

```
kid matches ^[a-z]+$
male>office>company matches ^[a-z]+$
female>url matches ^http(s)*:\/\/[a-z]+\.(com|in|net)$
```

## Example Array of Objects Config 

```
male : {
    url : https://asdf.com
    run : True
    mode : none
    age : 32.0234
    offices : [
        {
            location : blr
            desg : Architect
            company : amagi
        }
        {
            location : delhi
            desg : EM
            company : amagi
        }
    ]
}

female : {
     url : https://efgh.com
     run : True
     mode : none
     age : 31.0234
}

kid : qwerty
house : 148
dimensions : [25.0, 51.0, 25.0, 51.0]
```

## Example Array of Objects Validation 

```
kid matches ^[a-z]+$
male>offices>company matches ^[a-z]+$
female>url matches ^http(s)*:\/\/[a-z]+\.(com|in|net)$
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


