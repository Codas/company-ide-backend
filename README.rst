=====================================
 Company ide-backend
=====================================

.. contents:: Table of Contents
.. sectnum::

Overview
========

| `Company-mode`_ completion back-end for `haskell-mode`_ via `ide-backend-mode`_.
| It runs when the major mode is derived from `haskell-mode`_.
| This mode is based on the excellent `company-ghc`_ by Iku Iwasa.

Installation
============

Depends
-------
* cl-lib
* `company-mode`_
* `ide-backend-mode`_

  In order for ``company-ide-backend`` to work, ``ide-backend-mode`` has to be available and running.
  This is best done by calling ``ide-backend-mode-start``.

Setup from MELPA_
----------------
Note: This package is currently not available on MELPA, please use the git method for now.

..
   1. Install from `MELPA`_::

        M-x package-install RET company-ide-backend RET


   2. Add ``company-ide-backend`` to ``company-backends`` after loading
      `company-mode`_ and `ide-backend-mode`_.

      .. code:: emacs-lisp

        (add-to-list 'company-backends 'company-ide-backend)

      Or when using `spacemacs`_, disable company-ghc and add company-ide-backend:

      .. code:: emacs-lisp

        (setq-default dotspacemacs-excluded-packages '(company-ghc))
        (spacemacs|add-company-hook haskell-mode)
        (push '(company-ide-backend) company-backends-haskell-mode)

Setup from Git
--------------
1. Install from Git::

     git clone https://github.com/Codas/company-ide-backend.git

2. Add ``company-ide-backend`` to ``company-backends`` after loading
   `company-mode`_ and `ide-backend-mode`_.

   .. code:: emacs-lisp

     (add-to-list 'load-path "/path/to/company-ide-backend")
     (add-to-list 'company-backends 'company-ide-backend)

   Or when using `spacemacs`_:

   .. code:: emacs-lisp
      
     (add-to-list 'load-path "/path/to/company-ide-backend")
     (setq-default dotspacemacs-excluded-packages '(company-ghc))
     (spacemacs|add-company-hook haskell-mode)
     (push '(company-ide-backend) company-backends-haskell-mode)

             
Feature
=======

Completion
----------
The following completions are available.

1. Pragma names.

   .. image:: images/pragma.png
      :width: 415
      :height: 225
      :alt: Completion for pragma

2. Qualified imported keywords.

   .. image:: images/qualified.png
      :width: 415
      :height: 225
      :alt: Completion for qualified imported keywords

3. Keywords from imported modules.

   .. image:: images/keyword.png
      :width: 415
      :height: 225
      :alt: Completion for keywords of imported modules

Show type info in minibuffer
----------------------------
* Type information for certain completion candidates are displayed in the minibuffer.
  Currently, ide-backend only supplies type information for completion
  candidates if they are actively used in the current module or defined in the
  current cabal project.

  .. image:: images/showinfo.png
     :width: 415
     :height: 225
     :alt: Show info in minibuffer (``nomodule``)

Show module name as annotation
------------------------------
* Module name is displayed as completion annotation
  if ``company-ide-backend-show-module`` is non-nil (default) as in the above images.

Note
====
* Currently, company-ide-backend treats all symbols as completion prefix unless
  it starts from line beginning.  This means other back-ends after
  company-ide-backend have no chance to provide completion candidates in
  haskell-mode.

  As of now, if you want to use other back-ends with company-ide-backend, use
  grouped back-end like below.

  .. code:: emacs-lisp

     (add-to-list 'company-backends '(company-ide-backend :with company-dabbrev-code))

* ``company-ide-backend`` does not automatically reload the current file or
  interacts in any other way with `ide-backend-mode`_ except to gather completion candidates.
  In order to reload the current file, call ``ide-backend-mode-load``.

* This mode is based on the excellent `company-ghc`_ by Iku Iwasa. It is
  however not a fully functional replacement, as many more advanced features
  like import module completions, etc. are currently not available.


License
=======
Licensed under the GPL 3+ license.

.. _company-mode: http://company-mode.github.io/
.. _company-ghc: https://github.com/iquiw/company-ghc
.. _ide-backend-mode: https://github.com/chrisdone/ide-backend-mode
.. _haskell-mode: https://github.com/haskell/haskell-mode
.. _spacemacs: https://github.com/syl20bnr/spacemacs
.. _MELPA: http://melpa.milkbox.net/
