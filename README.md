# Rupaka (WIP)

## Format Description 

Config Format has the following 
  - d represents a define row which defines a Key -> Value pair 
  - di | ds represents additional type information on the define row , di - Integer Value , ds - String value
  - Similarly v represents a validation row and vi / vs  represets the type of the value 
  - A definition row has the format <d(i|s)> [Key] : [Value with Spaces] 
  - A validation row has the format <v(i|s)> [Key]  [Conditional (<|<=|>=|>|==)] [Limit value]


## Example Config 

```
di threads : 10
di users : 3
ds usertoken : asdfghjkl
```

## Example Validation 

```
vi threads < 15
vi users > 1

```
## Run 

```
./rupaka -c "example.cfg" -v "example.vld" -o "output.json"
``` 
