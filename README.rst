`wavetoy5 <https://github.com/eschnett/wavetoy5>`__
===================================================

|Build Status| |Coverage Status|

A progression of WaveToy implementations in Haskell. This is step 3,
where we allow different represenations for functions.

Problem description
-------------------

.. math::

   \partial_{tt} u = \partial_{xx} u

Build instructions
------------------

.. code:: sh

    # Build the project.
    stack build

    # Run the test suite.
    stack test

    # Run the benchmarks.
    stack bench

    # Generate documentation.
    stack haddock

.. |Build Status| image:: https://travis-ci.org/eschnett/wavetoy5.svg?branch=master
   :target: https://travis-ci.org/eschnett/wavetoy5
.. |Coverage Status| image:: https://coveralls.io/repos/github/eschnett/wavetoy5/badge.svg
   :target: https://coveralls.io/github/eschnett/wavetoy5
