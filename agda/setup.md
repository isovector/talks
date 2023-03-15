# Setup

## Installing Agda

### From a Package Manger

[See the documentation
here.](https://agda.readthedocs.io/en/latest/getting-started/installation.html#prebuilt-packages-and-system-specific-instructions)
Make sure your resultant version of Agda is `2.6.3`. If not, you will have to
build from source.

*You do not need to install the standard library for this talk.*


### From Source

First, install `stack` as [documented
here.](https://docs.haskellstack.org/en/stable/#how-to-install-stack). Then:

```bash
$ stack unpack Agda-2.6.3
$ cd Agda-2.6.3
$ stack --stack-yaml stack-8.10.7.yaml install
```

This will take about 30 minutes. If everything was successful, you should now
have `agda` on your PATH.

*You do not need to install the standard library for this talk.*


## Editor Integration

Agda is an interactive programming language, meaning that the compiler will help
you write code---both in terms of presenting information to you, and also in
terms of manipulating the AST. Thus, interacting with Agda requires editor
support. You can use any editor you'd like that supports "Agda-mode," though
tech-support will only be provided for VS Code, Vim and Emacs. If you don't have
a strong preference between these three, you should choose VS Code; Agda is
already more than enough to occupy your attention, and is not a good time to
also try to learn a new editor.

Create a new file `test.agda`:

```agda
module test where
```

which we will use to test the editor environment setup below.

### VS Code

In the menu, select `File > Preferences > Extensions`. Search for `agda-mode`,
and install it with the `Install` button.

Make sure you don't have the vim extension installed, as it conflicts with the
agda extension. Maybe you'd prefer just using `vim` instead?

**NOTE:** The current version of `agda-mode` has an unfortunate bug that
requires you to restart the Agda server every time you open a new file. You will
only need to do this once---at the beginning---during the talk. After opening a
new Agda file, invoke `View > Command Palette`. Search for `Agda: Quit and
restart` and run it. If everything was successful, you should get a big `All
Done` message popping up on the bottom of your screen.

Check out [the
documentation](https://github.com/banacorn/agda-mode-vscode)
if you need a hand.


### Vim

Agda-mode for **Neovim** is provided by
[cornelis](https://github.com/isovector/cornelis). Make sure you are running a
recent version of Neovim. Add the following to your
[vim-plug](https://github.com/junegunn/vim-plug) configuration, and then run
`:PluginInstall`.

```viml
Plug 'kana/vim-textobj-user'
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'liuchengxu/vim-which-key'
Plug 'isovector/cornelis', { 'do': 'stack build' }
```

Add the following configuration to your `.vimrc` file:


```viml
au BufRead,BufNewFile *.agda call AgdaFiletype()
function! AgdaFiletype()
  inoremap <localleader> <C-O>:call cornelis#prompt_input()<CR>
  nnoremap <buffer> <leader>l :CornelisLoad<CR>
  nnoremap <buffer> <leader>r :CornelisRefine<CR>
  nnoremap <buffer> <leader>d :CornelisMakeCase<CR>
  nnoremap <buffer> <leader>, :CornelisTypeContext<CR>
  nnoremap <buffer> <leader>. :CornelisTypeContextInfer<CR>
  nnoremap <buffer> <leader>n :CornelisSolve<CR>
  nnoremap <buffer> <leader>a :CornelisAuto<CR>
  nnoremap <buffer> gd        :CornelisGoToDefinition<CR>
  nnoremap <buffer> [/        :CornelisPrevGoal<CR>
  nnoremap <buffer> ]/        :CornelisNextGoal<CR>
  nnoremap <buffer> <C-A>     :CornelisInc<CR>
  nnoremap <buffer> <C-X>     :CornelisDec<CR>
endfunction
```

`:source` your `.vimrc` file. Open `test.agda`, and press `<leader>l`. If you
get a new split saying `All Done`, you're ready to go.

Check out [the
documentation](https://github.com/isovector/cornelis#readme)
if you need a hand.


### Emacs

Try running `agda-mode setup`. If this succeeds, you are all setup. If not, add
the following to your emacs configuration:

```elisp
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
```

Try running `C-c C-l` in `test.agda` to load Agda. If you get a message saying
`All Done`, you're ready to go.

Check out [the
documentation](https://agda.readthedocs.io/en/latest/getting-started/installation.html#step-3-running-the-agda-mode-program)
if you need a hand.

