# test

`type `**`A`**` {-q,+p} +r -s`  
doc:A


> **`MkA1`**`: (q -> s -> p) -> r -> A`  
> doc:MkA1
> 
> 
> **`MkA2`**`: p -> (s -> r) -> A`  
> doc:MkA2
> 
> 
`type `**`B`**  
doc:B


`subtype A Integer Unit Unit <: B`  

`type `**`RecA`**` {-q,+p} +r -s`  
doc:RecA


> **`MkRecA1`**`: (q -> s -> p) -> r -> RecA`  
> doc:MkRecA1
> 
> 
> **`MkRecA2`**`: p -> (s -> r) -> RecA`  
> doc:MkRecA2
> 
> 
`type `**`RecB`**  
doc:RecB


`subtype RecA Integer Unit Unit <: RecB`  

`type `**`R`**  
doc:R


> **`MkR`**`: R`  
> doc:MkR
> 
> 
> > **`rp`**`: Integer -> a -> a`  
> > doc:rp
> > 
> > 
> > **`rq`**`: Integer | Text`  
> > doc:rq
> > 
> > 
**`x`**`: Text`  
doc:x


**`y`**`: Text -> Text`  
doc:y


`namespace `**`N`**  
doc:N


> **`q`**`: Integer`  
> doc:N.q
> 
> 
> **`xx`**`: Text`  
> doc:N.xx
> 
> 
> `type `**`NmA`**` {-q,+p} +r -s`  
> doc:NmA
> 
> 
> > **`MkNmA1`**`: (q -> s -> p) -> r -> NmA`  
> > doc:MkNmA1
> > 
> > 
> > **`MkNmA2`**`: p -> (s -> r) -> NmA`  
> > doc:MkNmA2
> > 
> > 
> `type `**`NmB`**  
> doc:NmB
> 
> 
> > **`MkNmB`**`: Integer -> NmB`  
> > doc:MkNmB
> > 
> > 
> `subtype NmA Integer Unit Unit <: NmB`  
> 
> `subtype NmRecA Integer Unit Unit <: NmRecB`  
> 
> `type `**`NmRecA`**` {-q,+p} +r -s`  
> doc:NmRecA
> 
> 
> > **`MkNmRecA1`**`: (q -> s -> p) -> r -> NmRecA`  
> > doc:MkNmRecA1
> > 
> > 
> > **`MkNmRecA2`**`: p -> (s -> r) -> NmRecA`  
> > doc:MkNmRecA2
> > 
> > 
> `type `**`NmRecB`**  
> doc:NmRecB
> 
> 
> > **`MkNmRecB`**`: Integer -> NmRecB`  
> > doc:MkNmRecB
> > 
> > 
> `type `**`NmR`**  
> doc:NmR
> 
> 
> > **`MkNmR`**`: NmR`  
> > doc:MkNmR
> > 
> > 
> > > **`nmrp`**`: Integer -> a -> a`  
> > > doc:nmrp
> > > 
> > > 
> > > **`nmrq`**`: Integer | Text`  
> > > doc:nmrq
> > > 
> > > 
> `namespace `**`NN`**  
> 
> > **`xyz`**`: Unit`  
> > doc:N.NN.xyz
> > 
> > 
