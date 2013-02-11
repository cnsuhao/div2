\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{portfolio}[2013/02/01 Portfolio Class]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}

\ProcessOptions \relax

\LoadClass{report}


\RequirePackage{fullpage}
\RequirePackage{fancyhdr}
\RequirePackage{listings}
\RequirePackage{color}
\RequirePackage{xcolor}
\RequirePackage{caption}
\RequirePackage{subcaption}
\RequirePackage{graphicx}
\RequirePackage{fp}
\RequirePackage{pdfpages}


%%% Colors
\definecolor{dark-gray}{gray}{0.3}


%% Headers/Footers
\pagestyle{fancyplain}

\fancyhf{}
\renewcommand{\headrulewidth}{0pt}

\lfoot[\fancyplain{\thepage}{\thepage~Section~[\thesection]~[Chapter]}]{}
\rfoot[]{\fancyplain{\thepage}{\thepage}}


%%% Meta Info
\def\metaentry#1{%
  \begingroup
  \edef\@tempa{#1,\space}%
  \expandafter\endgroup
  \expandafter\readentry\@tempa\relax
}
\def\readentry#1, #2\relax{%
  \doentry{#1}%
  \begingroup
  \ifx\relax#2\relax % if #2 is empty
      \def\next{\endgroup\endmetaentry}
  \else
      \def\next{\endgroup\readentry#2\relax}%
  \fi
  \next
}
\def\doentry#1{& #1 \\}
\def\endmetaentry{}

\newcommand{\mitem}[2]{\vspace{-8pt}\\\textbf{:#1:} \metaentry{#2}}%

\newenvironment{meta}{\begin{tabular}{r l}}{\end{tabular}}


%%% Course Commands
\newcommand{\courseself}[0]{\subsection*{Self Evaluation}}
\newcommand{\coursedesc}[0]{\subsection*{Course Description}}
\newcommand{\coursegrade}[1]{\subsection*{Grade: #1}}
\newcommand{\courseeval}[3]{\newpage\subsection*{Evaluation: #1 (#2)\\#3}}


%%% Structure
\newcommand{\dividerpart}[1]{\part*{#1\addcontentsline{toc}{chapter}{~~\emph{#1}}}}


%%%  Images
\newcommand{\theimagesboundwidth}[0]{\textwidth}
\newcommand{\theimagescount}[0]{3}
\newcommand{\theimagesroot}[0]{}
\newcommand{\imagesboundwidth}[1]{\renewcommand{\theimagesboundwidth}{#1}}
\newcommand{\imagescount}[1]{\renewcommand{\theimagescount}{#1}}
\newcommand{\imagesroot}[1]{\renewcommand{\theimagesroot}{#1}}

\newcommand{\image}[2]{
    \FPdiv\result{1.0}{\theimagescount}
    \FPdiv\factor{\theimagescount}{100}
    \FPsub\result{\result}{\factor}
    \begin{subfigure}[b]{\result\textwidth}
        \centering
        \includegraphics[width=\theimagesboundwidth]{{\theimagesroot}/#1}
        \caption{\centering{#2}}
        \label{img:{#1}}
    \end{subfigure}
}


\newenvironment{images}[3]
{ \imagesroot{#1}
  \imagescount{#2}
  \imagesboundwidth{#3}
  \begin{figure}[h!] }
{ \end{figure} }
%\newcommand{\courseself}[0]{\subsection*{Self Evaluation}}


%%% Listings
\lstset{%
    basicstyle=\small,%
    numbers=left,%
    numberstyle=\small,%
    frame=single,%
    tabsize=4,%
    breakatwhitespace=true,%
    breaklines=true,%
    emptylines=2,%
    prebreak={...},%
}

\lstdefinelanguage{Javascript}{
    keywords={typeof, new, true, false, catch, function, return, null, catch, switch, var, if, in, while, do, else, case, break},
    keywordstyle=\bfseries,
    ndkeywords={\$, class, export, boolean, throw, implements, import, this},
    ndkeywordstyle=\bfseries,
    identifierstyle=\color{black},
    sensitive=false,
    comment=[l]{//},
    morecomment=[s]{/*}{*/},
    commentstyle=\ttfamily,
    stringstyle=\ttfamily,
    morestring=[b]',
    morestring=[b]"%
}

%\DeclareCaptionFont{white}{\color{white}}
%\DeclareCaptionFormat{listing}{\colorbox{gray}{\parbox{\textwidth}{#1#2#3}}}
%\captionsetup[lstlisting]{format=listing,labelfont=white,textfont=white}

