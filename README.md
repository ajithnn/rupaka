# Rupaka (WIP)

## Format Description 

Config Format has the following 
  - A file can either have validations or configurations, these need to be separately passed to the compiler.
  - num | str | bool represents additional type information on the row , num - Double Value , str - String value, bool - Boolean value
  - A definition file has the format <(num|str|bool|[num]|[bool]|[str])> [Key] : [Value with Spaces] 
  - A validation file has the format <(num|str|bool|[num]|[str]|[bool])> [Key]  [Conditional] (<|<=|>=|>|==|matches|oneof)] [Limit value]


## Example Config 

```
num threads : 10
num users : 3
str usertoken : asdfghjkl
```

## Example Validation 

```
num threads < 15
num users > 1

```
## Run 

```
./rupaka -c "example.cfg" -v "example.vld" -o "output.json"
``` 
