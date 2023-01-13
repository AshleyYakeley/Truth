# test

<code>type </code>**<code>A</code>**<code> {-q,+p} +r -s</code>

> doc:A
> 
> **<code>MkA1</code>**<code> : (q -&gt; s -&gt; p) -&gt; r -&gt; A</code>
> 
> > doc:MkA1
> > 
> **<code>MkA2</code>**<code> : p -&gt; (s -&gt; r) -&gt; A</code>
> 
> > doc:MkA2
> > 
<code>type </code>**<code>B</code>**

> doc:B
> 
<code>subtype A Integer Unit Unit &lt;: B</code>

<code>type </code>**<code>RecA</code>**<code> {-q,+p} +r -s</code>

> doc:RecA
> 
> **<code>MkRecA1</code>**<code> : (q -&gt; s -&gt; p) -&gt; r -&gt; RecA</code>
> 
> > doc:MkRecA1
> > 
> **<code>MkRecA2</code>**<code> : p -&gt; (s -&gt; r) -&gt; RecA</code>
> 
> > doc:MkRecA2
> > 
<code>type </code>**<code>RecB</code>**

> doc:RecB
> 
<code>subtype RecA Integer Unit Unit &lt;: RecB</code>

<code>type </code>**<code>R</code>**

> doc:R
> 
> **<code>MkR</code>**<code> : R</code>
> 
> > doc:MkR
> > 
> > **<code>rp</code>**<code> : Integer -&gt; a -&gt; a</code>
> > 
> > > doc:rp
> > > 
> > **<code>rq</code>**<code> : Integer | Text</code>
> > 
> > > doc:rq
> > > 
**<code>x</code>**<code> : Text</code>

> doc:x
> 
**<code>y</code>**<code> : Text -&gt; Text</code>

> doc:y
> 
<code>namespace </code>**<code>N</code>**

> doc:N
> 
> **<code>q.N</code>**<code> : Integer</code>
> 
> > doc:N.q
> > 
> **<code>xx.N</code>**<code> : Text</code>
> 
> > doc:N.xx
> > 
> <code>type </code>**<code>NmA.N</code>**<code> {-q,+p} +r -s</code>
> 
> > doc:NmA
> > 
> > **<code>MkNmA1.N</code>**<code> : (q -&gt; s -&gt; p) -&gt; r -&gt; NmA.N</code>
> > 
> > > doc:MkNmA1
> > > 
> > **<code>MkNmA2.N</code>**<code> : p -&gt; (s -&gt; r) -&gt; NmA.N</code>
> > 
> > > doc:MkNmA2
> > > 
> <code>type </code>**<code>NmB.N</code>**
> 
> > doc:NmB
> > 
> > **<code>MkNmB.N</code>**<code> : Integer -&gt; NmB.N</code>
> > 
> > > doc:MkNmB
> > > 
> <code>subtype NmA Integer Unit Unit &lt;: NmB</code>
> 
> <code>type </code>**<code>NmRecA.N</code>**<code> {-q,+p} +r -s</code>
> 
> > doc:NmRecA
> > 
> > **<code>MkNmRecA1.N</code>**<code> : (q -&gt; s -&gt; p) -&gt; r -&gt; NmRecA.N</code>
> > 
> > > doc:MkNmRecA1
> > > 
> > **<code>MkNmRecA2.N</code>**<code> : p -&gt; (s -&gt; r) -&gt; NmRecA.N</code>
> > 
> > > doc:MkNmRecA2
> > > 
> <code>type </code>**<code>NmRecB.N</code>**
> 
> > doc:NmRecB
> > 
> > **<code>MkNmRecB.N</code>**<code> : Integer -&gt; NmRecB.N</code>
> > 
> > > doc:MkNmRecB
> > > 
> <code>subtype NmRecA Integer Unit Unit &lt;: NmRecB</code>
> 
> <code>type </code>**<code>NmR.N</code>**
> 
> > doc:NmR
> > 
> > **<code>MkNmR.N</code>**<code> : NmR.N</code>
> > 
> > > doc:MkNmR
> > > 
> > > **<code>nmrp</code>**<code> : Integer -&gt; a -&gt; a</code>
> > > 
> > > > doc:nmrp
> > > > 
> > > **<code>nmrq</code>**<code> : Integer | Text</code>
> > > 
> > > > doc:nmrq
> > > > 
> <code>namespace </code>**<code>NN.N</code>**
> 
> > **<code>xyz.NN.N</code>**<code> : Unit</code>
> > 
> > > doc:N.NN.xyz
> > > 
