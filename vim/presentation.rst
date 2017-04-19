:title: Expert Level Vim
:data-transition-duration: 150

:css: fonts.css
:css: presentation.css

----

:id: title

.. raw:: html

  <h1>Expert Level Vim</h1>
  <h3>A talk by <span>Sandy Maguire</span></h3>
  <h4>reasonablypolymorphic.com</h4>

----

Vim and EMACS are the two most popular editors.

Of all time.

Why?

----

They introduced new ideas about what an editor should be.

----

EMACS
=====

"An editor should be extensible."

----

This lesson has been learned and adopted.

----

Vim
===

"An editor should be composable."

----

Doing it wrong.
===============

Atom's API provides the following

.. code:: javascript

  deleteToBeginningOfLine()
  deleteToBeginningOfSubword()
  deleteToBeginningOfWord()
  deleteToEndOfLine()
  deleteToEndOfSubword()
  deleteToEndOfWord()
  deleteToNextWordBoundary()
  deleteToPreviousWordBoundary()

----

Vim is a language.
==================

It has a small number of primitives that compose well together.

----

Motion.
=======

A motion moves you from one place to another.

----

Right.
======

.. raw:: html

  <pre class="buffer"><span class="cursor">h</span>ello, my foxy friend</pre>

  <div class="group">
  <blockquote>l</blockquote>
  <pre class="buffer">h<span class="cursor">e</span>llo, my foxy friend</pre>
  </div>

----

End of word.
============

.. raw:: html

  <pre class="buffer"><span class="cursor">h</span>ello, my foxy friend</pre>

  <div class="group">
  <blockquote>e</blockquote>
  <pre class="buffer">hell<span class="cursor">o</span>, my foxy friend</pre>
  </div>

----

Word.
=====

.. raw:: html

  <pre class="buffer"><span class="cursor">h</span>ello, my foxy friend</pre>

  <div class="group">
  <blockquote>w</blockquote>
  <pre class="buffer">hello<span class="cursor">,</span> my foxy friend</pre>
  </div>

----

WORD.
=====

.. raw:: html

  <pre class="buffer"><span class="cursor">h</span>ello, my foxy friend</pre>

  <div class="group">
  <blockquote>W</blockquote>
  <pre class="buffer">hello, <span class="cursor">m</span>y foxy friend</pre>
  </div>

----

unTil.
======

.. raw:: html

  <pre class="buffer"><span class="cursor">h</span>ello, my foxy friend</pre>

  <div class="group">
  <blockquote>tx</blockquote>
  <pre class="buffer">hello, my f<span class="cursor">o</span>xy friend</pre>
  </div>

----

Find.
=====

.. raw:: html

  <pre class="buffer"><span class="cursor">h</span>ello, my foxy friend</pre>

  <div class="group">
  <blockquote>fx</blockquote>
  <pre class="buffer">hello, my fo<span class="cursor">x</span>y friend</pre>
  </div>

----

End of line.
============

.. raw:: html

  <pre class="buffer"><span class="cursor">h</span>ello, my foxy friend</pre>

  <div class="group">
  <blockquote>$</blockquote>
  <pre class="buffer">hello, my foxy frien<span class="cursor">d</span></pre>
  </div>

----

Sentence.
=========

.. raw:: html

  <pre class="buffer">(This <span class="cursor">e</span>xample is
    (a little more) complicated. See?)
  </pre>


  <blockquote>)</blockquote>
  <pre class="buffer">(This example is
    (a little more) complicated. <span class="cursor">S</span>ee?)
  </pre>

----

Closing paren.
==============

.. raw:: html

  <pre class="buffer">(This <span class="cursor">e</span>xample is
    (a little more) complicated. See?)
  </pre>


  <blockquote>])</blockquote>
  <pre class="buffer">(This example is
    (a little more) complicated. See?<span class="cursor">)</span>
  </pre>

----

Search.
=======

.. raw:: html

  <pre class="buffer">(This <span class="cursor">e</span>xample is
    (a little more) complicated. See?)
  </pre>


  <blockquote>/ated</blockquote>
  <pre class="buffer">(This example is
    (a little more) complic<span class="cursor">a</span>ted. See?)
  </pre>

----

Going backwards.
================

All motions have a "backwards" version.

.. raw:: html

  <ul>
    <li><pre>b &larr; w</pre></li>
    <li><pre>( &larr; )</pre></li>
  </ul>

Unless there is an obvious subtitute, the backwards version is usually capitalized.

.. raw:: html

  <ul>
    <li><pre>F &larr; f</pre></li>
    <li><pre>? &larr; /</pre></li>
  </ul>

----

Backwards word.
===============

.. raw:: html

  <pre class="buffer">hello, my <span class="cursor">f</span>oxy friend</pre>

  <div class="group">
  <blockquote>b</blockquote>
  <pre class="buffer">hello, <span class="cursor">m</span>y foxy friend</pre>
  </div>


----

Counts.
=======

Additionally, motions can also take a **count**.

----

.. raw:: html

  <pre class="buffer space"><span class="cursor">t</span>here are lots of 'e's here</pre>

  <div class="group">
  <blockquote>fe</blockquote>
  <pre class="buffer">th<span class="cursor">e</span>re are lots of 'e's here</pre>
  </div>

  <div class="group">
  <blockquote>4fe</blockquote>
  <pre class="buffer">there are lots of '<span class="cursor">e</span>'s here</pre>
  </div>

----

Unsolicited motion advice.
==========================

.. raw:: html

  <span class="cursive bigger">You are doing it wrong</span> if you ever:

* use the arrow keys for *anything*.
* use `hjkl` to move around.

----

.. raw:: html

  <span class="bigger">Get in the habit of using the most <span class="cursive">parsimonious</span> motion.</span>

----

Parsi-motion-y.
===============

----

Learning more.
==============

.. raw:: html

  <blockquote>:help motion</blockquote>

for so many more.

You can also write your own!

----

Motions ain't everything.
=========================

We also have **operators**.

----

Operators.
==========

An operator is Vim's idea of a *targeted side-effect.*

It's a verb!

----

You already know some.
======================

----

Delete.
=======

.. raw:: html

  <blockquote>d&rarr;</blockquote>

"Delete something."

----

Delete something?
=================

What is "something?"

It's a **motion**!
------------------

----

.. raw:: html

  <pre class="buffer space"><span class="cursor">t</span>here are lots of 'e's here</pre>

  <div class="group">
  <blockquote>w</blockquote>
  <pre class="buffer">there <span class="cursor">a</span>re lots of 'e's here</pre>
  </div>

  <div class="group">
  <blockquote>dw</blockquote>
  <pre class="buffer"><span class="cursor">a</span>re lots of 'e's here</pre>
  </div>

----

.. raw:: html

  <pre class="buffer space"><span class="cursor">t</span>here are lots of 'e's here</pre>

  <div class="group">
  <blockquote>4fe</blockquote>
  <pre class="buffer">there are lots of '<span class="cursor">e</span>'s here</pre>
  </div>

  <div class="group">
  <blockquote>d4fe</blockquote>
  <pre class="buffer"><span class="cursor">'</span>s here</pre>
  </div>

----

.. raw:: html

  <pre class="buffer space">int x(bool <span class="cursor">f</span>oo, void* task) {</pre>

  <div class="group">
  <blockquote>])</blockquote>
  <pre class="buffer">int x(bool foo, void* task<span class="cursor">)</span> {</pre>
  </div>

  <div class="group">
  <blockquote>d])</blockquote>
  <pre class="buffer">int x(bool <span class="cursor">)</span> {</pre>
  </div>

----

More operators.
===============

There are several other operators.

----

Change.
=======

Delete some text and leave you in insert mode.

.. raw:: html

  <blockquote>c&rarr;</blockquote>

Use it to replace text.

----

Yank.
=====

Copy some text into the clipboard.

.. raw:: html

  <blockquote>y&rarr;</blockquote>

You can paste it later with

.. raw:: html

  <blockquote>p</blockquote>

----

.. raw:: html

  <pre class="buffer space">vim is <span class="cursor">v</span>ery cool</pre>

  <div class="group">
  <blockquote>ywP</blockquote>
  <pre class="buffer">vim is very<span class="cursor"> </span>very cool</pre>
  </div>

----

Automatic Yanking.
==================

Whenever you delete or change text, the text that disappeared is moved into your yank clipboard.

----

.. raw:: html

  <pre class="buffer space">vim is <span class="cursor">v</span>ery cool</pre>

  <div class="group">
  <blockquote>dw4P</blockquote>
  <pre class="buffer">vim is very very very very<span class="cursor"> </span>cool</pre>
  </div>

----

Surround text.
==============

.. raw:: html

  <blockquote>ys&rarr;&#9187;</blockquote>

Surround the text described by a motion with some character.

----

.. raw:: html

  <pre class="buffer space">wulky wilkenson is a <span class="cursor">p</span>ost-utopian</pre>

  <div class="group">
  <blockquote>ys$"</blockquote>
  <pre class="buffer">wulky wilkenson is a "<span class="cursor">p</span>ost-utopian"</pre>
  </div>

----

.. raw:: html

  <pre class="buffer space">10 - <span class="cursor">6</span> - 4</pre>

  <div class="group">
  <blockquote>ysf4)</blockquote>
  <pre class="buffer">10 - (<span class="cursor">6</span> - 4)</pre>
  </div>

  <div class="group">
  <blockquote>ysf4(</blockquote>
  <pre class="buffer">10 - ( <span class="cursor">6</span> - 4 )</pre>
  </div>

In general, the "open" character inserts spaces.

----

.. raw:: html

  <pre class="buffer space">marve<span class="cursor">l</span>ous</pre>

  <div class="group">
  <blockquote>ysw&lt;span&gt;</blockquote>
  <pre class="buffer">marve&lt;span&gt;<span class="cursor">l</span>ous&lt;/span&gt;</pre>
  </div>



----

Reindent code.
==============

.. raw:: html

  <blockquote>=&#9187;</blockquote>

----

.. raw:: html

  <pre class="buffer"><span class="cursor">i</span>nt blah() {
  return 0;
  }</pre>

  <blockquote>=G</blockquote>

  <pre class="buffer space"><span class="cursor">i</span>nt blah() {
      return 0;
  }</pre>

Doesn't work super well for Haskell, unfortunately.

----

Learning more.
==============

.. raw:: html

  <blockquote>:help operators</blockquote>

for so many more.

You can also write your own!

----

Text Objects.
=============

----

**Text objects** are somewhere between motions and filetype-specific syntax.

----

They let you describe things like:

* all text inside this HTML tag
* the code at this level of indentation
* one portion of a *snake_case_identifier*

A text object is always contiguous text.

----

Motions vs text objects.
========================

You can't **move** by a text object, but you can **operate** on one.

----

"An argument" text object.
==========================

.. raw:: html

  <pre class="buffer space">mconcat [a, <span class="cursor">b</span>, c]</pre>

  <div class="group">
  <blockquote>&#10803;aa</blockquote>
  <pre class="buffer">mconcat [a<span class="tobj">, b</span>, c]</pre>
  </div>

  <div class="group">
  <blockquote>daa</blockquote>
  <pre class="buffer">mconcat [a<span class="cursor">,</span> c]</pre>
  </div>

----

Inside and around.
==================

Most text objects target **inside** (excluding) or **around** (including) some structure.

.. raw:: html

  <pre class="buffer space">mconcat ["hello", "<span class="cursor">g</span>oodbye"]</pre>

  <div class="group">
  <blockquote>&#10803;i"</blockquote>
  <pre class="buffer">mconcat ["hello", "<span class="tobj">goodbye</span>"]</pre>
  </div>

  <div class="group">
  <blockquote>&#10803;a"</blockquote>
  <pre class="buffer">mconcat ["hello", <span class="tobj">"goodbye"</span>]</pre>
  </div>

----

.. raw:: html

  <pre class="buffer space">mconcat ["hello", "<span class="cursor">g</span>oodbye"]</pre>

  <div class="group">
  <blockquote>&#10803;i]</blockquote>
  <pre class="buffer">mconcat [<span class="tobj">"hello", "goodbye"</span>]</pre>
  </div>

  <div class="group">
  <blockquote>&#10803;a]</blockquote>
  <pre class="buffer">mconcat <span class="tobj">["hello", "goodbye"]</span></pre>
  </div>

----

Text objects can help if your cursor isn't in the right position for a motion.

.. raw:: html

  <pre class="buffer space">I will bring do<span class="cursor">o</span>m cookies</pre>

  <div class="group">
  <blockquote>dw</blockquote>
  <pre class="buffer">I will bring do<span class="cursor">c</span>ookies</pre>
  </div>

  <div class="group">
  <blockquote>daw</blockquote>
  <pre class="buffer">I will bring <span class="cursor">c</span>ookies</pre>
  </div>


----

Learning more.
==============

.. raw:: html

  <blockquote>:help text-obj</blockquote>

for so many more.

You can also write your own!

----

Using the best operation is valuable.
=====================================

Why? Composition!

----

Repeat.
=======

.. raw:: html

  <blockquote>.</blockquote>

Perform the last operation again.

----

The wrong operation for the job.
================================

.. raw:: html

  <pre class="buffer"><span class="cursor">f</span>ooBar fooBar</pre>

  <blockquote>dtBiqux&uarr;</blockquote>
  <pre class="buffer">qu<span class="cursor">x</span>Bar fooBar</pre>

  <blockquote>w.</blockquote>
  <pre class="buffer">quxBar qu<span class="cursor">x</span>fooBar</pre>

----

Doing it right.
===============

.. raw:: html

  <pre class="buffer"><span class="cursor">f</span>ooBar fooBar</pre>

  <blockquote>civqux&uarr;</blockquote>
  <pre class="buffer">qu<span class="cursor">x</span>Bar fooBar</pre>

  <blockquote>w.</blockquote>
  <pre class="buffer">quxBar qu<span class="cursor">x</span>Bar</pre>

----

Spend 0% of your time in insert mode.
=====================================

Do exactly what you need in insert mode, and no more.

----

Develop the habit of pressing escape
====================================

every time you've finished a thought.
=====================================

----

The do's and don'ts of Visual Mode.
===================================

----

Don't.
======

(with some caveats)

----

Visual block mode.
==================

.. raw:: html

  <pre class="buffer"><span class="cursor">d</span>ata Enum = Foo
            | Bar
            | Baz
  </pre>
  <blockquote>&#9393;f=l2j</blockquote>
  <pre class="buffer"><span class="cursor">data Enum = </span>Foo
  <span class="cursor">          | </span>Bar
  <span class="cursor">          | </span>Baz
  </pre>

  <blockquote>x</blockquote>

  <pre class="buffer"><span class="cursor">F</span>oo
  Bar
  Baz
  </pre>

----

Visual block mode (cont).
=========================

.. raw:: html

  <blockquote>gv</blockquote>

  <pre class="buffer"><span class="cursor">Foo</span>
  <span class="cursor">Bar</span>
  <span class="cursor">Baz</span>
  </pre>

  <blockquote>I, &uarr;</blockquote>

  <pre class="buffer"><span class="cursor">,</span> Foo
  , Bar
  , Baz
  </pre>

----

Visual line mode.
=================

.. raw:: html

  <pre class="buffer">import Data.Map
  imp<span class="cursor">o</span>rt Data.List
  import Control.Monad

  -- don't touch me
  </pre>

  <blockquote>Vip</blockquote>

  <pre class="buffer">
  <span class="cursor">import Data.Map</span>
  <span class="cursor">import Data.List</span>
  <span class="cursor">import Control.Monad</span>

  -- don't touch me
  </pre>

  <blockquote>:sort&crarr;</blockquote>

  <pre class="buffer">
  <span class="cursor">i</span>mport Control.Monad
  import Data.List
  import Data.Map

  -- don't touch me
  </pre>

----

Registers.
==========

**Registers** are generalizations of the copy/paste clipboard.

They're "variables" you can stick text into.

----

Registers are named like this:

.. raw:: html

  <blockquote>"a</blockquote>
  <blockquote>"b</blockquote>
  <blockquote>"c</blockquote>

There are lots of registers available!

----

They can be combined with operators:

.. raw:: html

  <blockquote>"r&#10803;</blockquote>

----

.. raw:: html

  <pre class="buffer">three <span class="cursor">t</span>wo one</pre>
  <blockquote>"bdaw</blockquote>
  <pre class="buffer">three <span class="cursor">o</span>ne</pre>
  <blockquote>"adaw</blockquote>
  <pre class="buffer">thre<span class="cursor">e</span></pre>
  <blockquote>0"aP"bp</blockquote>
  <pre class="buffer"> onetwo<span class="cursor"> </span>three</pre>

----

Use registers to keep track of lots of pieces of text simultaneously!

----

There is an implicit, default register used if you do not specify one.

.. raw:: html

  <blockquote>""</blockquote>

This is where autoyanked text goes.

----

Insert mode?
============

You can work with registers in insert mode by pressing.

.. raw:: html

  <blockquote>&#9389;</blockquote>

----

Learning more.
==============

.. raw:: html

  <blockquote>:help registers</blockquote>

for all of the magic registers (like automatic math evaluation!)

----

Record.
=======

.. raw:: html

  <blockquote>q&#9187;</blockquote>

  <p>Records your keystrokes and puts them into register &#9187;.</p>

Press **q** to exit again.

----

What use is this?
=================

It's a code-to-data transformation.

Make the edits you want, and export/reuse them!

----

Moving keystrokes into the buffer.
==================================

.. raw:: html

  <pre class="buffer"><span class="cursor"> </span></pre>

  <blockquote>qziHello world!&uarr;ddq</blockquote>
  <pre class="buffer"><span class="cursor"> </span></pre>

  <blockquote>"zp</blockquote>
  <pre class="buffer">iHello world!&uarr;d<span class="cursor">d</span></pre>

----

This doesn't seem super useful.
===============================

(it's not, on its own)

----

Run macro.
==========

.. raw:: html

  <blockquote>@&#9187;</blockquote>

The inverse transformation! Run data as vim commands!

----

Better than alcoholism.
=======================

.. raw:: html

  <pre class="buffer"><span class="cursor">9</span>9 bottles of beer on the wall!</pre>

  <blockquote>qbyyp&#9395;q</blockquote>
  <pre class="buffer">99 bottles of beer on the wall!
  9<span class="cursor">8</span> bottles of beer on the wall!</pre>

  <blockquote>98@b</blockquote>

  <pre class="buffer">99 bottles of beer on the wall!
  98 bottles of beer on the wall!
  97 bottles of beer on the wall!
  ...
  1 bottles of beer on the wall!
  <span class="cursor">0</span> bottles of beer on the wall!</pre>

----

Keep macros in mind when you're doing repetitive, mechanical yet non-trivial edits.

----

Thanks for listening!
=====================

Questions?
==========

