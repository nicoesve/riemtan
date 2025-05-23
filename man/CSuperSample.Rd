% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/super_sample.R
\name{CSuperSample}
\alias{CSuperSample}
\title{CSuperSample Class}
\description{
This class represents a collection of CSample objects (samples of connectomes), providing methods to aggregate, analyze, and compute statistics across multiple samples sharing the same Riemannian metric.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{list_of_samples}}{The list of CSample objects aggregated in this super-sample.}

\item{\code{sample_size}}{The total number of connectomes in all samples.}

\item{\code{matrix_size}}{The size of the connectome matrices.}

\item{\code{mfd_dim}}{The dimension of the manifold.}

\item{\code{riem_metric}}{The Riemannian metric used by all samples.}

\item{\code{variation}}{The total variation of the aggregated sample.}

\item{\code{sample_cov}}{The sample covariance matrix of the aggregated sample.}

\item{\code{full_sample}}{The aggregated CSample object containing all connectomes.}

\item{\code{frechet_mean}}{The Frechet mean of the aggregated sample.}

\item{\code{Within}}{The within-group covariance matrix (W).}

\item{\code{Total}}{The total covariance matrix (T).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CSuperSample-new}{\code{CSuperSample$new()}}
\item \href{#method-CSuperSample-compute_variation}{\code{CSuperSample$compute_variation()}}
\item \href{#method-CSuperSample-compute_sample_cov}{\code{CSuperSample$compute_sample_cov()}}
\item \href{#method-CSuperSample-gather}{\code{CSuperSample$gather()}}
\item \href{#method-CSuperSample-compute_fmean}{\code{CSuperSample$compute_fmean()}}
\item \href{#method-CSuperSample-compute_W}{\code{CSuperSample$compute_W()}}
\item \href{#method-CSuperSample-compute_T}{\code{CSuperSample$compute_T()}}
\item \href{#method-CSuperSample-clone}{\code{CSuperSample$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CSuperSample-new"></a>}}
\if{latex}{\out{\hypertarget{method-CSuperSample-new}{}}}
\subsection{Method \code{new()}}{
Initialize a CSuperSample object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CSuperSample$new(samples)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{samples}}{A list of CSample objects. All must use the same Riemannian metric.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{CSuperSample} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CSuperSample-compute_variation"></a>}}
\if{latex}{\out{\hypertarget{method-CSuperSample-compute_variation}{}}}
\subsection{Method \code{compute_variation()}}{
Compute the total variation of the aggregated sample.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CSuperSample$compute_variation()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. This function is called for its side effects. The result is stored in the \code{variation} active binding.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CSuperSample-compute_sample_cov"></a>}}
\if{latex}{\out{\hypertarget{method-CSuperSample-compute_sample_cov}{}}}
\subsection{Method \code{compute_sample_cov()}}{
Compute the sample covariance matrix of the aggregated sample.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CSuperSample$compute_sample_cov()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. This function is called for its side effects. The result is stored in the \code{sample_cov} active binding.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CSuperSample-gather"></a>}}
\if{latex}{\out{\hypertarget{method-CSuperSample-gather}{}}}
\subsection{Method \code{gather()}}{
Gather all connectomes from the list of samples into a single CSample object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CSuperSample$gather()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. This function is called for its side effects. The result is stored in the \code{full_sample} active binding.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CSuperSample-compute_fmean"></a>}}
\if{latex}{\out{\hypertarget{method-CSuperSample-compute_fmean}{}}}
\subsection{Method \code{compute_fmean()}}{
Compute the Frechet mean of the aggregated sample.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CSuperSample$compute_fmean()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. This function is called for its side effects. The result is stored in the \code{frechet_mean} active binding.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CSuperSample-compute_W"></a>}}
\if{latex}{\out{\hypertarget{method-CSuperSample-compute_W}{}}}
\subsection{Method \code{compute_W()}}{
Compute the within-group covariance matrix (W) for the samples.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CSuperSample$compute_W()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. This function is called for its side effects. The result is stored in the \code{Within} active binding.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CSuperSample-compute_T"></a>}}
\if{latex}{\out{\hypertarget{method-CSuperSample-compute_T}{}}}
\subsection{Method \code{compute_T()}}{
Compute the total covariance matrix (T) for the samples.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CSuperSample$compute_T()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. This function is called for its side effects. The result is stored in the \code{Total} active binding.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CSuperSample-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CSuperSample-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CSuperSample$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
