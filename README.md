# style_of_criticism

This repository contains the code and lexicons for the stylometric analysis of the "Style of criticism."<br/>
Because of copyright and privacy issues, we cannot share the corpora that we used for our analyses.<br/>
The main statistics can be found here:<br/>

```
                                     |  Social Reading  |  Paper Magazines  |  Scientific Journals
-------------------------------------|------------------|-------------------|-----------------------------------------------------------
Source                               |  aNobii          |  Sole 24 Ore      |  Between – Osservatorio critico della germanistica - OBLIO
Publication dates                    |                  |  2010-2011        |  1998-2016
Total number of tokens               |  646964          |  704242           |  655192
Length of shortest review (tokens)   |  1               |  32               |  234
Length of longest review (tokens)    |  2229            |  1840             |  3195
Mean length of reviews (tokens)      |  125.3174        |  526.5012         |  1197.275
Standard deviation of review length  |  169.9646        |  350.4949         |  459.4581

```

The <b>"Extend_lexicons.py"</b> file contains a script for expanding the lexicons in the "lexicons" folder through the fastText Italian Pre-trained word vector: https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md<br/>
The <b>Style_of_criticism_step1.R</b> file contains a script for the analysis of the texts in the "corpus" folder (NOTE: in this repository they are just empty files), through stylomety and word classification. It also generates files to be analyzed with the LIWC software: https://liwc.wpengine.com/<br/>
The <b>Style_of_criticism_step2.R</b> file contains an SVM classifier, that works on the results of the previous analysis and evaluate the quality of the classification.
