\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{portfolio}[2013/02/01 Portfolio Class]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}

\ProcessOptions \relax

\LoadClass{report}

\RequirePackage{fancyhdr}
\RequirePackage{caption}
\RequirePackage{subcaption}
\RequirePackage{graphicx}
\RequirePackage{fp}

\usepackage{sty/codelsting}
\usepackage{sty/meta}
\usepackage{sty/texpath}
\usepackage{sty/course}



%%% Headers/Footers
%\pagestyle{fancyplain}
%
%
%\fancyhf{}
%\renewcommand{\headrulewidth}{0pt}
%
%\lfoot[\fancyplain{\thepage~\thepath}{\thepage~\thepath}]{\fancyplain{\thepage~\thepath}{\thepage~\thepath}}
%\rfoot[]{\fancyplain{\thepage}{\thepage}}




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
