\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{draft}[2020/06/26 Class for working paper drafts]

%% We start with an article class with 12 point font.
%% We allow other options such as titlepage to be passed.
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[12pt]{article}

%% We want one inch margins
\RequirePackage{geometry}
\geometry{margin = 1in}

%% And double-spaced text, except for tables and figures
\RequirePackage{setspace}
\doublespacing
\RequirePackage{etoolbox}
\AtBeginEnvironment{table}{\singlespacing}
\AtEndEnvironment{table}{\doublespacing}
\AtBeginEnvironment{figure}{\singlespacing}
\AtEndEnvironment{figure}{\doublespacing}

%% Title page
\RequirePackage{titling}
\renewcommand\abstract[1]{\def\@abstract{#1}}
\newcommand{\maketitlepage}{%
\singlespacing
\begin{titlingpage}
    \maketitle
    \begin{center}
        \textbf{Abstract}
    \end{center}
    \begin{quote}
        \@abstract
    \end{quote}
\end{titlingpage}
\doublespacing
}

