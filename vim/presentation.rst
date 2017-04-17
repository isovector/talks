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

<!-- TODO(sandy): most of these have semantic names -->

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

Delete surrounding.
===================

.. raw:: html

  <blockquote>ds&#9187;</blockquote>

Doesn't take a motion, but still pretty sweet.

----

.. raw:: html

  <pre class="buffer space">shouldnt be 'quote<span class="cursor">d</span>'</pre>

  <div class="group">
  <blockquote>ds'</blockquote>
  <pre class="buffer">shouldnt be <span class="cursor">q</span>uoted</pre>
  </div>

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

Most text-objects target **inside** (excluding) or **around** (including) some structure.

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

Thanks for listening!
=====================

Questions?
==========

