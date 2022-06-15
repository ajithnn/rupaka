# Rupaka (WIP)


## Stages - Design A


### Generate Validator

1. Load Validation rules file during compilation 
2. Generate binary with validations built-in 


## Validate Configurations 

1. Pass configuration files to the validator binary 
2. Validates and converts the config to reqd output formats


## Stages - Design B


### Generate configs


1. Use compile binary to take validations and configs and arguments.
2. Parse Validations and configs. 
3. Validate the configs as required.
4. Convert to output format as required. 



## Stages - Design C


### Run gRPC service 

1. Run a gRPC service which exposes 2 functions A,B 
2. function 'A' takes a validation file and generates a validatorID 
3. function 'B' takes a validatorID and configuration file , and returns an error or output json 

