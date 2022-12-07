# test
**`x`** `: Text`  
doc:x


`type` **`B`**  
doc:B


`type` **`A`** `{-q,+p}` `+r` `-s`  
doc:A


> **`MkA1`** `: q -> s -> p -> r -> A`  
> doc:MkA1
> 

> **`MkA2`** `: p -> s -> r -> A`  
> doc:MkA2
> 

`subtype` `A Integer Unit Unit` `<:` `B`  

`subtype` `RecA Integer Unit Unit` `<:` `RecB`  

`type` **`RecA`** `{-q,+p}` `+r` `-s`  
doc:RecA


> **`MkRecA1`** `: q -> s -> p -> r -> RecA`  
> doc:MkRecA1
> 

> **`MkRecA2`** `: p -> s -> r -> RecA`  
> doc:MkRecA2
> 

`type` **`RecB`**  
doc:RecB


> **`MkRecB`** `: Integer -> RecB`  
> doc:MkRecB
> 

`type` **`R`**  
doc:R


> **`MkR`** `: R`  
> doc:MkR
>
> > rp: Integer -> a -> a;
> > rq: Integer | Text;

**`y`** `: Text -> Text`  
doc:y


`namespace` **`N`**


> **`q`** `: Integer`  
> doc:N.q


> **`xx`** `: Text`  
> doc:N.xx


> `type` **`NmA`** `{-q,+p}` `+r` `-s`  
> doc:NmA


> > **`MkNmA1`** `: q -> s -> p -> r -> A`  
> > doc:MkNmRecA1
> > 

> > **`MkA2`** `: p -> s -> r -> A`  
> > doc:MkA2
> > 

> `type` **`NmB`**  
> doc:NmB


> > **`MkNmB`** `: Integer -> NmB`  
> > doc:MkNmB
> > 

> `subtype` `NmA Integer Unit Unit` `<:` `NmB`  

> `subtype` `NmRecA Integer Unit Unit` `<:` `NmRecB`  

> `type` **`NmRecA`** `{-q,+p}` `+r` `-s`  
> doc:NmRecA


> > **`MkNmRecA1`** `: q -> s -> p -> r -> NmRecA`  
> > doc:MkNmRecA1
> > 

> > **`MkNmRecA2`** `: p -> s -> r -> NmRecA`  
> > doc:MkNmRecA2
> > 

> `type` **`NmRecB`**  
> doc:NmRecB


> > **`MkNmRecB`** `: Integer -> NmRecB`  
> > doc:MkNmRecB
> > 

> `type` **`NmR`**  
> doc:NmR


> > **`MkNmR`** `: NmR`  
> > doc:MkNmR
> >
> > > nmrp: Integer -> a -> a;
> > > nmrq: Integer | Text;

