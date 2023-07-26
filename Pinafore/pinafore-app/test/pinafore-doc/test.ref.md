# test

<code>type </code>**<code>A\.</code>**<code> {\-q,\+p} \+r \-s</code>

> doc:A
> 
> **<code>Mk1\.A\.</code>**<code> : (q \-\> s \-\> p) \-\> r \-\> A\.</code>
> 
> > doc:Mk1.A
> > 
> **<code>Mk2\.A\.</code>**<code> : p \-\> (s \-\> r) \-\> A\.</code>
> 
> > doc:Mk2.A
> > 
<code>type </code>**<code>B\.</code>**

> doc:B
> 
<code>subtype A Integer Unit Unit \<: B</code>

<code>type </code>**<code>RecA\.</code>**<code> {\-q,\+p} \+r \-s</code>

> doc:RecA
> 
> **<code>Mk1\.RecA\.</code>**<code> : (q \-\> s \-\> p) \-\> r \-\> RecA\.</code>
> 
> > doc:Mk1.RecA
> > 
> **<code>Mk2\.RecA\.</code>**<code> : p \-\> (s \-\> r) \-\> RecA\.</code>
> 
> > doc:Mk2.RecA
> > 
<code>type </code>**<code>RecB\.</code>**

> doc:RecB
> 
<code>subtype RecA Integer Unit Unit \<: RecB</code>

<code>type </code>**<code>R\.</code>**

> doc:R
> 
> **<code>Mk\.R\.</code>**<code> : R\.</code>
> 
> > doc:Mk.R
> > 
> > **<code>rp</code>**<code> : Integer \-\> a \-\> a</code>
> > 
> > > doc:rp
> > > 
> > **<code>rq</code>**<code> : Integer \| Text</code>
> > 
> > > doc:rq
> > > 
**<code>x\.</code>**<code> : Text</code>

> doc:x
> 
**<code>y\.</code>**<code> : Text \-\> Text</code>

> doc:y
> 
## N\.

> doc:N
> 
**<code>q\.N\.</code>**<code> : Integer</code>

> doc:N.q
> 
**<code>xx\.N\.</code>**<code> : Text</code>

> doc:N.xx
> 
<code>type </code>**<code>NmA\.N\.</code>**<code> {\-q,\+p} \+r \-s</code>

> doc:NmA
> 
> **<code>Mk1\.NmA\.N\.</code>**<code> : (q \-\> s \-\> p) \-\> r \-\> NmA\.N\.</code>
> 
> > doc:Mk1.NmA
> > 
> **<code>Mk2\.NmA\.N\.</code>**<code> : p \-\> (s \-\> r) \-\> NmA\.N\.</code>
> 
> > doc:Mk2.NmA
> > 
<code>type </code>**<code>NmB\.N\.</code>**

> doc:NmB
> 
> **<code>Mk\.NmB\.N\.</code>**<code> : Integer \-\> NmB\.N\.</code>
> 
> > doc:Mk.NmB
> > 
<code>subtype NmA Integer Unit Unit \<: NmB</code>

<code>type </code>**<code>NmRecA\.N\.</code>**<code> {\-q,\+p} \+r \-s</code>

> doc:NmRecA
> 
> **<code>Mk1\.NmRecA\.N\.</code>**<code> : (q \-\> s \-\> p) \-\> r \-\> NmRecA\.N\.</code>
> 
> > doc:Mk1.NmRecA
> > 
> **<code>Mk2\.NmRecA\.N\.</code>**<code> : p \-\> (s \-\> r) \-\> NmRecA\.N\.</code>
> 
> > doc:Mk2.NmRecA
> > 
<code>type </code>**<code>NmRecB\.N\.</code>**

> doc:NmRecB
> 
> **<code>Mk\.NmRecB\.N\.</code>**<code> : Integer \-\> NmRecB\.N\.</code>
> 
> > doc:Mk.NmRecB
> > 
<code>subtype NmRecA Integer Unit Unit \<: NmRecB</code>

<code>type </code>**<code>NmR\.N\.</code>**

> doc:NmR
> 
> **<code>Mk\.NmR\.N\.</code>**<code> : NmR\.N\.</code>
> 
> > doc:Mk.NmR
> > 
> > **<code>nmrp</code>**<code> : Integer \-\> a \-\> a</code>
> > 
> > > doc:nmrp
> > > 
> > **<code>nmrq</code>**<code> : Integer \| Text</code>
> > 
> > > doc:nmrq
> > > 
### NN\.N\.

**<code>xyz\.NN\.N\.</code>**<code> : Unit</code>

> doc:N.NN.xyz
> 
