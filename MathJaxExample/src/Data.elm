module Data exposing (initialText)


initialText =
    """
\\title{Harmonic Oscillator}

|| mathmacros
\\newcommand{\\bra}[0]{\\langle}
\\newcommand{\\ket}[0]{\\rangle}
\\newcommand{\\set}[1]{\\{#1\\}}

| banner
\\ilink{Quantum Mechanics Notes jxxcarlson:quantum-mechanics-notes}


\\setcounter{6}

\\tags{quantum-mechanics, jxxcarlson:harmonic-oscillator}

\\contents

\\section{Introduction}



\\image{http://psurl.s3.amazonaws.com/images/jc/mass_spring-fdac.png}{Mass-spring system}{width:200}


The prototypical harmonic oscillator is classical mechanics is given by the mass-spring system, as in the figure.  If $m$ is the mass and $k$ is the spring constant, then the equation of motion is

\\begin{equation}
m\\ddot x + ky = 0
\\end{equation}

The system vibrates at an angular frequency given by

\\begin{equation}
\\omega^2 = \\frac{k}{m}
\\end{equation}

The Hamiltonian function for this system is∂and 

\\begin{equation}
  H = K.E. + P.E. = \\frac{1}{2}m\\dot x^2 + \\frac{1}{2}kx^2 
\\end{equation}

For the passage to quantum mechanics, we write it as

\\begin{equation}
  H =  \\frac{p^2}{2m} + \\frac{m\\omega^2 x^2  }{2}
\\end{equation}

The time-independent form of the Schroedinger equation is therefore

\\begin{equation}
H\\phi =E\\phi
\\end{equation}

where the quantum-mechanical Hamiltonian is

\\begin{equation}
  H =  \\frac{\\hat p^2}{2m} + \\frac{m\\omega^2 x^2  }{2}
\\end{equation}

In more detail,

\\begin{equation}
- \\frac{\\hbar^2}{2m} \\frac{d^2 \\phi}{dx^2}
  + \\frac{m\\omega^2 x^2  }{2} = E\\phi
\\end{equation}


This ordinary differential equation is of the form $L\\phi = E\\phi$
where $L$ is a positive, self-adjoint operator.  
While the eigenvalues and eigenfunctions
can be found explicitly by the method of power series,
the algebraic method described below gives more insight into the nature and structure of the solutions.

\\section{Factorizing the Hamiltonian}

While we can find both energies and eigenfunctions using power series, there is, however, a more conceptual path.  The Hamiltonian can be written as a constant times a sum of squares, namely

\\begin{equation}
H =  \\frac{1}{2m}\\left( \\hat p^2  + m^2 \\omega^2 x^2 \\right)
\\end{equation}

If we were doing high-school algebra, we could factor the Hamiltonian as

\\begin{equation}
H = \\frac{1}{2m}
      \\left(\\omega \\hat x - i \\hat p \\right)
   \\left(\\omega \\hat x + i \\hat p \\right)
  =  \\frac{1}{2m} A^\\dagger A
\\end{equation}

Alas, we cannot do this because the operations of multiplication by $x$ and differentiation by $x$ \\emph{do not commute}. Indeed,

\\begin{equation}
  \\left[\\frac{d}{dx}, x\\right] = 1
\\end{equation}

so that

\\begin{equation}
\\label{uncertainty_principle}
  \\left[\\hat p, x\\right] = -i \\hbar
\\end{equation}


This relation is a form of the Heisenberg Uncertainty Principle.  
Let us therefore re-calculate out trial factorization my 
multiplying out the expression $A^\\dagger A/2m$:

\\begin{aligned}
\\frac{1}{2m}A^\\dagger A 
  &= \\frac{1}{2m}\\left[ (m\\omega x - i\\hat p)(m\\omega x + i \\hat p)\\right] \\ 
  &= \\frac{1}{2m}\\left[ (m^2 \\omega^2 x^2+ \\hat p^2) - im\\omega [\\hat p, x] \\right] 
\\end{aligned}

Using the Uncertainty Principle \\emph{qua} commutator relation, we have

\\begin{equation}
\\label{AdaggerA}
\\frac{1}{2m}A^\\dagger A = H - \\frac{\\omega\\hbar}{2}
\\end{equation}

With $A$ and $A^\\dagger$ in the opposite order, we have

\\begin{equation}
\\label{AAdagger}
\\frac{1}{2m}A A^\\dagger = H + \\frac{\\omega\\hbar}{2}
\\end{equation}

Write

\\begin{equation}
\\label{hohamiltonian_number_operator}
A^\\dagger = \\sqrt{2m\\omega\\hbar}\\, a^\\dagger
\\end{equation}

and define $a$ in the same way by rescaling  $A$. 
These are the  \\term{creation} and \\term{annihilation} operators:

\\begin{equation}
\\label{creation_op}
a^\\dagger = \\frac{m\\omega x - i\\hat p}{\\sqrt{2m\\omega\\hbar}}
\\end{equation}

and 

\\begin{equation}
\\label{annihilation_op}
a = \\frac{m\\omega x + i\\hat p}{\\sqrt{2m\\omega\\hbar}}
\\end{equation}

From \\eqref{AdaggerA}, we find an expression for the Hamiltonian
in terms of these operators:


\\begin{equation}
\\label{hfactorplus}
H = \\omega\\hbar\\left(a^\\dagger a +  \\frac{1}{2}\\right)
\\end{equation}

and \\eqref{AAdagger} yields

\\begin{equation}
\\label{hfactorminus}
H = \\omega\\hbar\\left(a a^\\dagger -  \\frac{1}{2}\\right)
\\end{equation}

Taking the difference of the last two relations, one obtains

\\begin{equation}
  \\label{aadaggercommutation}
  \\[a,a^\\dagger\\] = 1.





Thus the Hamiltonian can be written as the sum of $(\\hbar\\omega)a^\\dagger a$ and $\\hbar\\omega /2$, where the operators in the first term do not commute.



\\section{Eigenvalues}

Let us now exploit our decomposition of the Hamiltonian.  For this we will use the standard Hermitian inner product for complex-valued functions of the real line,

\\begin{equation}
\\bra \\phi, \\psi \\ket  = \\int_{-\\infty}^{\\infty} \\phi(x)\\overline{\\psi(x)} dx,
\\end{equation}

and we will work in the Hilbert space $L^2(\\mathbb{R})$ of functions for which the associated norm is finite -- the space of square-integrable functions.  A function will be square integrable if it decays rapidly enough at infinity.  For example, a polynomial times a Gaussian $e^{-cx^2/4}$ is square-integrable.



Using
\\eqref{hfactorplus}, we find that

\\begin{aligned}
\\bra H\\phi, \\phi\\ket 
  &= \\hbar\\omega(\\bra a^\\dagger a \\phi, \\phi \\ket + (1/2)\\bra \\phi, \\psi \\ket) \\
  &= \\hbar \\omega(||a\\phi||^2 + (1/2)||\\phi||^2)
\\end{aligned}

Thus, if $H\\phi = \\lambda\\phi$, then (recalling that $\\lambda$ is real, we have

\\begin{equation}
\\lambda \\ge \\hbar\\omega/2
\\end{equation}

Thus the closure of the set of eigenvalues is a a closed set of real numbers with least element $\\lambda^* \\ge \\hbar\\omega/2$.  Since $\\lambda^*$ is, \\emph{a priori} not an eigenvalue but rather a limit of eigenvalues, there is an eigenvalue $\\lambda_0 \\ge \\lambda^*$ that satisfies $\\lambda_0 < \\hbar\\omega$.  

| lemma
If $\\phi_\\lambda$ is an eigenvector of $H$ with eigenvalue $\\lambda$, then either (i) $a\\phi_\\lambda = 0$ or (b) $a\\phi_\\lambda$ is an eigenvector with eigenvalue $\\lambda - \\hbar\\omega$.


The proof depends on the commutation relation

\\begin{equation}
[H,a] = -\\hbar\\omega a
\\end{equation}

It follows form the basic commutation relation $[a,a^\\dagger] = 1$:

\\begin{aligned}
[H,a] &= \\hbar\\omega[a^\\dagger a, a] = \\hbar\\omega(a^\\dagger a a - a a^\\dagger a) \\
  &= \\hbar\\omega(a^\\dagger a a - (a^\\dagger  aa + [a^\\dagger, a] a)) \\
 &= -\\hbar\\omega a
\\end{aligned}

Returning to the lemma, we have

\\begin{aligned}
Ha\\phi_\\lambda &= a H\\phi_\\lambda + [H,a]\\phi_\\lambda \\
  &= \\lambda a \\phi_\\lambda - \\hbar\\omega a \\phi_\\lambda \\
 &= (\\lambda - \\hbar\\omega)\\phi_\\lambda
\\end{aligned}

Thus, if $a\\phi_\\lambda$ is nonzero, it is an eigenvector with
eigenvalue $\\lambda - \\hbar\\omega$. Q.E.D.

Now let $\\phi_0$ be an eigenvector with eigenvalue $\\lambda_0$
as above.footnote:[We are abusing notation.  If $n$ is an integer, then it stands for a label -- a quantum number -- for the wave function $\\phi_n$, 
which then has eigenvalue $\\lambda_n$.]  The vector $a\\phi_0$ is either zero or an eigenvector
of the Hamiltonian with eigenvalue $\\lambda_0 - \\hbar\\omega$.
But $\\lambda_0 - \\hbar\\omega < \\lambda^*$, and so
$a\\phi_0$ must be zero.  We conclude the following:


| theorem
The greatest lower bound for the spectrum of the 
harmonic oscillator is (i) an isolated point, (ii) an eigenvalue, (iii)
has the value $\\hbar\\omega/2$.

Only the last assertion requires proof.  Let $\\phi_0$ be an eigenvector with least eigenvalue.  Then $a\\phi_0 = 0$.  To find its eigenvalue, simply apply the Hamiltonian operator:


\\begin{equation}
H\\phi_0 = \\hbar\\omega(a^\\dagger a + 1/2)\\phi_0 = \\frac{\\hbar\\omega}{2}\\phi_0 
\\end{equation}


\\begin{theorem}
  The spectrum of the harmonic oscillator is the set of integers

  $$
    E_n = \\hbar\\omega\\left(n + \\frac{1}{2}\\right)
  $$

  The vectors $\\phi_n = (a^\\dagger)^n \\phi_0$ are eigenvectors 
  with eigenvalue $E_n$.
\\end{theorem}


| theorem
If $\\phi$ is an eigenfunction of $H$ with 
eigenvalue $E$, the $a^\\dagger \\phi$ is an eigenfunction
with eigenvalue $E + \\hbar\\omega$.


*Proof:* Suppose $H\\phi = E\\phi$. Then

$$
  Ha^\\dagger \\phi = a^\\dagger H\\phi + [H, a^\\dagger ]\\phi
$$

One shows that 

$$
  [H,a^\\dagger]  = \\hbar\\omega
$$

Therefore

$$
   Ha^\\dagger \\phi = a^\\dagger E\\phi + \\hbar\\omega \\phi
  = (E + \\hbar\\omega)a^\\dagger  \\phi
$$

*Q.E.D.*

It is conventional to call the operator

\\begin{equation}
 N = a^\\dagger a
\\end{equation}

the \\term{number operator}.  Note that $H = \\hbar\\omega(N + 1/2)$ Since $H\\phi_0 = (\\hbar\\omega/2)\\phi_0$, it
follows that $N\\phi_0$.    For the next highest energy state,

\\begin{equation}
Nu_1 = (a^\\dagger a)a^\\dagger  \\phi_0 = a^\\dagger a^\\dagger a \\phi_0 + a^\\dagger [a, a^\\dagger]\\phi_0 =\\phi_1
\\end{equation}

Now, proceeding by induction, one shows that

\\begin{equation}
   N\\phi_n = n\\phi_n.
\\end{equation}

One may think of $\\phi_n$ as an $n$-particle system, and one may think of $a^\\dagger$ and $a$ as operators that create and destroy particles.


\\begin{remark}
In several of the above computations we implicitly used the idea of *normal order*, in which products of $a$ and $a^\\dagger$ are rewritten using the identity $[a,a^\\dagger] = 1$  as a sum of terms in which all the creation operators are on the left and all the annihilation operators are on the right. We will encounter this process again when we do perturbation theory.
\\end{remark}


\\section{Wave Functions}

One might ask after all this algebra -- where are the wave functions?  To answer this, consider the ground state $\\phi_0$.  It satisfies the relation $a\\phi_0 = 0$.  But this is a first order differential equation, namely

\\begin{equation}
(m\\omega x + i\\hat p)\\psi(x) = 0
\\end{equation}

Writing it out, we have

\\begin{equation}
  \\frac{d\\phi}{dx} = -\\frac{m\\omega}{\\hbar} x \\phi
\\end{equation}

In still better form,

\\begin{equation}
  \\frac{d\\log\\phi}{dx} = -\\frac{m\\omega}{\\hbar} x 
\\end{equation}

so that

\\begin{equation}
  \\phi(x) = Ce^{ -m\\omega  x^2/2  \\hbar}
\\end{equation}

Thus the ground state wave function of the harmonic oscillator is a Gaussian.  For the higher energy states $\\phi_n$, apply the operator $(a^\\dagger)^n$ to the ground state.  As an example, we compute $a^\\dagger \\phi_0$ using

\\begin{equation}
a^\\dagger = m\\omega x - i\\hat p = m\\omega x - \\hbar\\frac{d}{dx}
\\end{equation}

Thus,

\\begin{equation}
\\phi_1(x) = 2m\\omega x e^{-m\\omega x^2/2\\hbar} 
\\end{equation}

which we write as

\\begin{equation}
\\phi_1(x) = m\\omega H_1(x)  e^{-m\\omega x^2/2\\hbar},
\\end{equation}

where $H_1(x) = 2x$. In general, the $n$-th eigenfunction is a polynomial of degree $n$ times a Gaussian,

\\begin{equation}
  \\phi_n(x) = m\\omega H_n(x)e^{ -m\\omega  x^2/2  \\hbar}
\\end{equation}

and the polynomials are the \\term{Hermite polynomials}.  

Proceeding in the same way, one can discover the wave functions $\\phi_n$ for arbitrary $n$, that is, one can find the Hermite polynomals

\\begin{aligned}
H_0 &= 1 \\
H_1 &= 2x \\
H_2 &= 4x^2 - 2 \\
H_3 &= 8x^3 - 12x \\
H^4 &= 8x^3 - 48x^2 + 12 \\
etc
\\end{aligned}

Note that the set of functions $\\set{\\phi_n}$ is orthogonal, since its elements belong to distinct eigenvalues of a Hermitian operator.  It is also a \\emph{complete} orthogonal system, meaning any wave function can be expressed as a Fourier series

\\begin{equation}
\\phi = \\sum_n c_n \\phi_n,
\\end{equation}

where 

\\begin{equation}
c_n = \\frac{\\langle \\phi, \\phi_n \\rangle}{||\\phi_n||^2}
\\end{equation}


\\section{Pictures}

Let $\\psi)n(x)$ be a normalized pure state with energy $E_n = m\\hbar \\omega(n + 1/2)$.
The images below are graphs of the probability densities $|\\psi_n(x)|^2$ for various $n$.  The ground state $\\psi_0$ has a single hump, $\\psi_1$ has two, and so on. Viewing a particle as a localized region of high probability density, we interpret $\\psi_n$ as a state consisting of $n+1$ particles.  It is for this reason that $a^\\dagger$ is called the creation operator: when applied to an $n$-particle state, it produces an $(n+1)$-particle state.


\\image{http://psurl.s3.amazonaws.com/images/jc/harmonic_oscillator_wave_functions-f79d.png}{Harmonic Oscillator}{width: 400, align: center}


There are other lessons to be learned from thinking about these graphs.  Consider for a moment the corresponding classical harmonic oscillator. 
We have

\\begin{equation}
\\frac{p^2}{2m} + \\frac{m\\omega^2 x^2}{2} = E,
\\end{equation}

where $E$ is the total energy.  Since all quantities are real, the squares are non-negative, and so we have an upper bound on $x$:

\\begin{equation}
|x| \\le \\left(\\frac{2E}{ n\\omega^2 }\\right)^{1/2}
\\end{equation}

Larger values of $x$ are forbidden.  Now consider any one of the pure states $\\psi_n(x)$. The wave function is a polynomial times a decaying exponential.  Let $x_L$ be the the maximum of the absolute values of the zeros of $\\psi_n(x)$.  Then for $|x| > x_L$, the wave function, although it decays exponentially.  There is a small but nonzero probability that the the particle be found in this region, and so there is a nonzero probability that it be found in the classically forbidden region.  

In the figure above, the dotted line represents the probability that the corresponding classical harmonic oscillator is found at a given position.  notice that as the occupation number $n$ increases, the quantum probability approaches the classical one in a certain sense: the "upper profile" of the latter approaches the former inside the classically permitted region.  What is quite different about the quantum distribution is (a) the fact that it oscillates and (b) its "leakage" outside of the classical region.


\\section{Normalization}

In computing Fourier expansions, it is convenient to have in hand normalized wavefunctions, that is,  eigefunctions $\\psi_n$ for which $||\\psi_n||^2 = 1$. 
We have in hand the eigenfunctions $\\phi_n = (a^\\dagger )^n\\phi_0$.  We must find constants $A_n$ such that $|A_n|^2||\\phi_n||^2 = 1$.  For the ground state, this is easy.  The integral in 

\\begin{equation}
|A_0|^2 \\int_{-\\infty}^\\infty e^{-m\\omega x^2/\\hbar}dx = 1
\\end{equation}

is a standard Gaussian, namely

\\begin{equation}
\\int_{\\infty}^\\infty e^{ -ax^2} dx = \\sqrt{\\frac{\\pi}{a}}
\\end{equation}

From it we find that

\\begin{equation}
A_0 = \\left(\\frac{m\\omega}{\\pi\\hbar}\\right)^{1/4}
\\end{equation}

so that

\\begin{equation}
\\psi_0(x) = \\left(\\frac{m\\omega}{\\pi\\hbar}\\right)^{1/4} e^{-m\\omega x^2/\\hbar}
\\end{equation}

The other normalization constants can be computed using our operator calculus.
Note that $a^\\dagger \\psi_n$ is a scalar multiple of $\\psi_n$, so we can write $a^\\dagger\\psi_n = c_n\\psi_n$. Then

\\begin{aligned}
\\bra a^\\dagger \\psi, a^\\dagger \\psi_n \\ket &= c_n^2 \\
&= \\bra aa^\\dagger \\psi_n, \\psi_n \\ket> \\
&= (n+1)||\\psi_n|^2
\\end{aligned}

where we have used XX.  Thus

\\begin{equation}
\\label{raise}
a^+ \\psi_n = \\sqrt{n+1} \\psi_{n+1}
\\end{equation}

In like manner, we find that

\\begin{equation}
a \\psi_n = \\sqrt{n} \\psi_{n-1}
\\end{equation}

Let us find the normalizations for the first few wave functions.  From \\eqref{raise}, with $n = 0$, we find that

\\begin{equation}
\\psi_1 = a^\\dagger \\psi_0
\\end{equation}

From \\eqref{raise}, with $n = 1$, we find that

\\begin{equation}
\\psi_2 = \\frac{1}{\\sqrt 2} a^\\dagger \\psi_1 
 = \\frac{1}{\\sqrt 2} ( a^\\dagger )^2 \\psi_0
\\end{equation}

From \\eqref{raise}, with $n = 2$, we find that

\\begin{equation}
\\psi_3 = \\frac{1}{\\sqrt 3} a^\\dagger \\psi_2 =  \\frac{1}{\\sqrt 3!} ( a^\\dagger )^3 \\psi_0
\\end{equation}

And by induction we find that

\\begin{equation}
\\psi_n =  \\frac{1}{\\sqrt n!} ( a^\\dagger )^n \\psi_0
\\end{equation}


\\section{Diatomic molecules}


\\image{https://dpotoyan.github.io/Chem324/images/osc-2.jpeg Diatomic molecule width:300}

A diatomic molecule like $\\text{H}_2$ or $\\text{HCl}$ can be idealized as a mass-spring system, that is, as a harmonic oscillator.  The bond joining the two atoms is the spring, and the atoms are the masses.  It is best to look at this system in a xref::230[center-of-mass reference frame]. That is, the coordinate frame moves uniformly with respect to the rest frame in such a way that the center of mass is motionless. The relevant dynamical variable is then $x = x_1 - x_2$, the separation between masses $m_1$ and $m_2$.  Newton's second law then reads

\\begin{equation}
\\mu \\ddot x = F,
\\end{equation}

where 

\\begin{equation}
\\mu = \\frac{m_1m_2}{m_1 + m_2}
\\end{equation}

is the \\term{reduced mass}.  Quantum-mechanically such a system is governed by the Schroedinger equation 

\\begin{equation}
\\frac{\\hat p^2 }{2\\mu} + V(x)
\\end{equation}

The potential $V(x)$ can be expanded in a Taylor series; by suitable coordinate changes, we may assume that it begins with a quadratic term.  If we neglect higher order terms, we are left with the harmonic oscillator:
 

\\begin{equation}
\\frac{\\hat p^2 }{2\\mu} + \\frac{\\mu\\omega^2 }{2} x^2
\\end{equation}

The higher order terms do have an effect; we will treat this matter in the next section, on the anharmonic oscillator.


\\subheading{HCl absorption spectrum}

\\image{http://psurl.s3.amazonaws.com/images/jc/hcl_spectral_energies-4194.png}{HCl Absorption Spectrum}{width:400, align:center}

The energy spectrum is $E_n =\\omega \\hbar (n + 1/2)$.
If this is so (in spite of the approximations made), then there is a prediction which can be checked with experiment.  Pass a monochromatic beam of electromagnetic radiation through a diatomic gas.  We consider the case of $\\text{HCl}$.  At frequencies with energy $E_n$ photons in the beam will interact with the molecule in the ground state and  raise it to the $n$-th state.  In such circumstances, beam energy is absorbed.  The  harmonic oscillator model predicts that these absorption frequencies be evenly spaced. The chart below is derived from the data in the figure (http://infohost.nmt.edu/~jaltig/HCl.pdf[source]). Numbers in the second column are wave numbers of absorbed radiation.  The number in column two, row $n$ results from absorption of a photon by an $\\text{HCl}$ molecule which bumps it from the ground state to the $n$-th state.  The first point is that wave numbers $k$ are proportional to energy: from $E = \\hbar \\omega$ and $\\omega/k = c$, we deduce $E = \\hbar k c$.  The second is that the observed wavenumber are indeed evenly spaced, up to a modest deviation.  In the table, the second differences are no more than 10 percent of the first differences.


|| comment
\\begin{indent}
\\begin{tabular}{|c|c|c|c|}
n & Wave number   & Difference & 2nd Diff  \\
1 &  2885.98      &      -      &     -      \\
2 &  5567.98      &  2682.00    &     -     \\
3 &  8346.78      &  2778.80    &    96.8  \\
4 & 10922.81      &  2576.03   &   -202.77 \\
5 & 13396.19      &  2473.38   &   -102.65  \\
\\end{tabular}
\\end{indent}

|| datatable
n____, Wave number, Difference, 2nd Diff
1, 2885.98, - , -
2 , 5567.98, 2682.00, -
3,  8346.78      ,  2778.80   ,    96.8
4 , 10922.81      ,  2576.03,   -202.77
5 , 13396.19      ,  2473.38   ,   -102.65

This is not bad considering that the harmonic oscillator model leaves out the fact that (a) there could be (and indeed are) cubic and higher terms in the Taylor expansion of the potential, (b) a diatomic molecule can rotate about its center of mass, and these rotational modes can be coupled to the vibrational ones.  As a result of these and other factors, the absorption spectrum shows more an more detail as the resolution of the spectroscopy increases.  At low resolution, the absorption spectrum is a series of almost evenly spaced peaks.  At somewhat higher resolution, the peaks split in two, forming a "doublet."  A still higher resolutions the peak reveals an almost comb-like structure.



\\image{http://psurl.s3.amazonaws.com/images/jc/hcl_fine_structure-b4e6.png}{Fine structure}{width: 400, align: center}

.Molecular force constant
The lowest vibrational energy transition of chem::[HCl] occurs at $\\lambda^{-1} = 2.89 \\times 10^{5} \\text{m}^{-1}$, corresponding to a frequency of $8.67\\times 10^{13} \\text{ Hertz}$. We can use this to estimate the length the bond between chem::[H] and chem::[Cl] by viewing the molecule as a classical mass-spring system vibrating at that frequency.  From the relations $\\omega^2 = k/\\mu$ and $\\omega = 2\\pi c/\\lambda$, we find that the spring constant is given by

\\begin{equation}
k = 4\\pi^2 c^2 \\lambda^{-2}\\mu
\\end{equation}

For the masses, we have $m_H = 1.00794\\text{ amu}$ and  $m_{Cl} = 35.453\\text{ amu}$, so that the reduced mass is $\\mu = 0.973\\text{ amu}$.  Since one atomic mass unit is $1.61\\times 10^{-27}\\text{ kg}$, we find that $k = 478\\text{ N/m}$.  As we shall see when we consider the angular momentum operator, the bond length can be inferred from the rotational spectrum.

NOTE: In the references on the chem::[HCl] spectrum, wavenumber, usually in inverse centimeters, refers to inverse wavelength $\\lambda^{-1}$, not the wavenumber of the expression $e^{ikx}$.


\\section{References}


\\href{http://infohost.nmt.edu/~jaltig/HCl.pdf}{Detailed description of HCl spectrum}

\\href{http://hyperphysics.phy-astr.gsu.edu/hbase/molecule/vibrot.html}{Vibrational and rotational energies of HCl}


\\href{http://ocw.mit.edu/courses/chemistry/5-61-physical-chemistry-fall-2007/lecture-notes/lecture35.pdf}{Vibrational spectroscopy}



\\href{http://www4.ncsu.edu/~franzen/public_html/CH431/lecture/lec_5.pdf}{Chemistry 431}

\\href{http://www2.chem.uic.edu/chem343/Manuals/MainLabs/FTIR_F12.pdf}{Vibrational-rotational spectrum of HCl}

\\href{http://www.phys.ufl.edu/courses/phy4803L/group_III/infra_red/irspec.pdf}{Vibrational-rotational spectrum of HCl}

\\href{http://inside.mines.edu/~lwiencke/PH300/F12/Quantum/L22-post.pdf}{SHO notes}

\\href{https://www.math.u-psud.fr/~helffer/m2bucarest2010.pdf}{Spectral theory - bucaresti}

\\href{http://isites.harvard.edu/fs/docs/icb.topic815933.files/Completeness-3Oct2010-4.pdf}{Jaffe, Completeness}

\\href{http://homepage.univie.ac.at/reinhold.bertlmann/pdfs/T2_Skript_Ch_5.pdf}{Coherent states}

\\href{http://hitoshi.berkeley.edu/221a/coherentstate.pdf}{More on coherent states}


\\href{http://depa.fquim.unam.mx/amyd/archivero/DiatomicMolecule_1314.pdf}{Intro to Molecular spectroscopy}


"""
