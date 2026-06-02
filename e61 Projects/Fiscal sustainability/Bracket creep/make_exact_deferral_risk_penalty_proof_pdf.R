library(grid)

output_pdf <- "exact_deferral_risk_penalty_proof.pdf"

wrap_lines <- function(text, width = 92) {
  out <- character()
  for (paragraph in strsplit(text, "\n", fixed = TRUE)[[1]]) {
    if (nchar(trimws(paragraph)) == 0) {
      out <- c(out, "")
    } else {
      out <- c(out, strwrap(paragraph, width = width))
    }
  }
  out
}

draw_page <- function(title, body) {
  grid.newpage()
  grid.text(
    title,
    x = unit(0.06, "npc"),
    y = unit(0.94, "npc"),
    just = c("left", "top"),
    gp = gpar(fontfamily = "Times", fontsize = 15, fontface = "bold")
  )

  lines <- wrap_lines(body)
  y <- 0.88
  line_height <- 0.029

  for (line in lines) {
    if (nchar(line) == 0) {
      y <- y - line_height * 0.75
    } else {
      grid.text(
        line,
        x = unit(0.06, "npc"),
        y = unit(y, "npc"),
        just = c("left", "top"),
        gp = gpar(fontfamily = "Courier", fontsize = 9.2)
      )
      y <- y - line_height
    }
  }
}

pdf(output_pdf, width = 8.27, height = 11.69, onefile = TRUE)

draw_page(
  "Exact Condition for a Tax Penalty on Risky Business Formation",
  paste(
    "Purpose.",
    "This note gives an exact condition, not a Taylor approximation, for when",
    "taxation makes a risky business less attractive than safe work after the",
    "business success payoff has been calibrated to pre-tax indifference.",
    "",
    "The proof is deliberately written in terms of an arbitrary after-tax payoff",
    "mapping. This covers a simple realised capital-gain tax, a terminal fully",
    "franked dividend, and a corporate share sale with full or partial franking",
    "credit capitalisation.",
    "",
    "Notation.",
    "B       background wealth, B > 0.",
    "W       pre-tax terminal accumulation from safe work.",
    "W_T     after-tax terminal accumulation from safe work.",
    "pf      probability of business failure.",
    "pm      probability of the same-as-safe business outcome.",
    "ph      probability of high success, ph = 1 - pf - pm > 0.",
    "S       high-success payoff, chosen by pre-tax indifference.",
    "u       increasing utility over total wealth; CRRA is specialised later.",
    "A(x)    terminal after-tax business payoff when the pre-tax payoff is x.",
    "",
    "For example, under a realised capital-gain tax, A(x) = x - T(x). Under",
    "the imputation script, A(x) is the state-specific after-tax amount generated",
    "by company tax, franking credits, dividend extraction, or share-sale treatment.",
    "",
    "Assume the failure payoff receives no refund: A(0) = 0.",
    sep = "\n"
  )
)

draw_page(
  "1. Pre-Tax Calibration",
  paste(
    "The risky business has pre-tax incremental payoffs",
    "",
    "    X = 0   with probability pf,",
    "    X = W   with probability pm,",
    "    X = S   with probability ph.",
    "",
    "The success payoff S is chosen so that the risky business has the same",
    "certainty equivalent as safe work before tax:",
    "",
    "    pf u(B) + pm u(B + W) + ph u(B + S) = u(B + W).      (1)",
    "",
    "Rearranging gives",
    "",
    "    ph [u(B + S) - u(B + W)] = pf [u(B + W) - u(B)].     (2)",
    "",
    "The same-as-safe probability pm does not appear directly in (2), because",
    "that state gives exactly the benchmark payoff W. It still matters through",
    "ph = 1 - pf - pm. If pm rises while pf is held fixed, ph falls and the",
    "required success payoff generally rises.",
    "",
    "Equation (2) is exact. It says that the probability-weighted utility gain",
    "from success exactly offsets the probability-weighted utility loss from",
    "failure.",
    sep = "\n"
  )
)

draw_page(
  "2. Exact After-Tax Penalty Condition",
  paste(
    "The after-tax expected utility of the risky business is",
    "",
    "    EU_A = pf u(B) + pm u(B + A(W)) + ph u(B + A(S)).    (3)",
    "",
    "Let CE_A be its after-tax certainty equivalent:",
    "",
    "    u(B + CE_A) = EU_A.",
    "",
    "The risky business is penalised relative to safe work exactly when",
    "",
    "    CE_A < W_T.",
    "",
    "Because u is increasing, this is equivalent to",
    "",
    "    EU_A < u(B + W_T).                                  (4)",
    "",
    "Now use the pre-tax indifference equation (1) to substitute for pf u(B):",
    "",
    "    pf u(B) = u(B + W) - pm u(B + W) - ph u(B + S).",
    "",
    "Substituting into (3) gives",
    "",
    "    EU_A = u(B + W)",
    "           - pm [u(B + W) - u(B + A(W))]",
    "           - ph [u(B + S) - u(B + A(S))].                (5)",
    "",
    "Combining (4) and (5), the exact necessary and sufficient condition for",
    "an after-tax penalty on the risky business is",
    "",
    "    pm [u(B + W) - u(B + A(W))]",
    "  + ph [u(B + S) - u(B + A(S))]",
    "      > u(B + W) - u(B + W_T).                          (6)",
    "",
    "This is the exact condition. No Taylor expansion has been used.",
    sep = "\n"
  )
)

draw_page(
  "3. Interpretation of the Exact Condition",
  paste(
    "Define the utility loss from taxing business payoff x as",
    "",
    "    L_A(x) = u(B + x) - u(B + A(x)).",
    "",
    "Define the utility loss from taxing safe work as",
    "",
    "    L_safe = u(B + W) - u(B + W_T).",
    "",
    "Then (6) can be written compactly as",
    "",
    "    pm L_A(W) + ph L_A(S) > L_safe.                      (7)",
    "",
    "This is exact and has a simple meaning:",
    "",
    "    The risky business is penalised when the probability-weighted utility",
    "    loss from taxing the same and success states exceeds the utility loss",
    "    from taxing the safe-work path.",
    "",
    "Important implications.",
    "",
    "1. The failure state drops out because A(0) = 0 and the same failure term",
    "   appears before and after tax.",
    "",
    "2. The same-as-safe state enters at the benchmark wealth level. It is not",
    "   discounted by the marginal utility of a huge success payoff.",
    "",
    "3. The success-state tax burden is exact utility loss L_A(S). In a first",
    "   order approximation it is downweighted by marginal utility at B + S,",
    "   but condition (7) itself does not rely on that approximation.",
    "",
    "4. The condition applies to imputation if A(W) and A(S) are interpreted as",
    "   the after-tax terminal business payoffs produced by the imputation",
    "   treatment in the code.",
    sep = "\n"
  )
)

draw_page(
  "4. CRRA Specialisation",
  paste(
    "For CRRA utility, gamma > 0:",
    "",
    "    u(w) = w^(1 - gamma) / (1 - gamma),   gamma != 1,",
    "    u(w) = log(w),                        gamma = 1.",
    "",
    "The calibrated success payoff is exact.",
    "",
    "For gamma != 1, let a = 1 - gamma. From (2):",
    "",
    "    (B + S)^a = (B + W)^a",
    "                + (pf / ph) [(B + W)^a - B^a].           (8)",
    "",
    "If the right-hand side is in the range of w^a for w > B + W, then",
    "",
    "    S = { (B + W)^a + (pf / ph) [(B + W)^a - B^a] }^(1/a) - B.",
    "",
    "For log utility:",
    "",
    "    log(B + S) = log(B + W)",
    "                 + (pf / ph) [log(B + W) - log(B)],      (9)",
    "",
    "so",
    "",
    "    S = exp(log(B + W) + (pf / ph)[log(B + W) - log(B)]) - B.",
    "",
    "The exact penalty condition is obtained by substituting CRRA utility into",
    "(6). For gamma != 1:",
    "",
    "    pm * { (B+W)^a - (B+A(W))^a } / a",
    "  + ph * { (B+S)^a - (B+A(S))^a } / a",
    "      > { (B+W)^a - (B+W_T)^a } / a.                    (10)",
    "",
    "For gamma = 1:",
    "",
    "    pm * log((B+W)/(B+A(W)))",
    "  + ph * log((B+S)/(B+A(S)))",
    "      > log((B+W)/(B+W_T)).                              (11)",
    "",
    "Equations (10) and (11) are exact.",
    sep = "\n"
  )
)

draw_page(
  "5. Relation to the Approximate Condition",
  paste(
    "The earlier condition with marginal-utility weights is a first-order",
    "approximation to the exact condition.",
    "",
    "Suppose A(x) = x - T(x). For small tax wedges,",
    "",
    "    u(B + x) - u(B + x - T(x)) approx u'(B + x) T(x).",
    "",
    "Apply this to (6):",
    "",
    "    pm u'(B+W) T(W) + ph u'(B+S) T(S)",
    "      > u(B+W) - u(B+W_T).",
    "",
    "Dividing by u'(B+W) gives",
    "",
    "    pm T(W) + ph [u'(B+S)/u'(B+W)] T(S)",
    "      > [u(B+W) - u(B+W_T)] / u'(B+W).                  (12)",
    "",
    "For CRRA,",
    "",
    "    u'(B+S) / u'(B+W) = ((B+W)/(B+S))^gamma.",
    "",
    "So the approximate condition becomes",
    "",
    "    pm T(W) + ph ((B+W)/(B+S))^gamma T(S)",
    "      > [u(B+W) - u(B+W_T)] / u'(B+W).                  (13)",
    "",
    "If the safe-work tax wedge is also linearised, the right-hand side is",
    "approximately W - W_T. This is useful intuition, but it is not the exact",
    "condition. The exact condition is (6), or (10)/(11) under CRRA.",
    sep = "\n"
  )
)

draw_page(
  "6. What the Exact Proof Does and Does Not Imply",
  paste(
    "The exact proof supports the central intuition:",
    "",
    "    A progressive penalty can dominate deferral only if the utility loss",
    "    from taxing the risky business states is large enough to exceed the",
    "    utility loss from taxing the safe-work benchmark.",
    "",
    "The proof also clarifies the role of the same-as-safe state:",
    "",
    "    With pm > 0, the term pm L_A(W) enters exactly. This term is evaluated",
    "    at the benchmark wealth level, so it can matter even when the success",
    "    payoff S is so large that success-state tax dollars have low marginal",
    "    utility.",
    "",
    "The proof does not imply a universal gamma threshold. Statements such as",
    "'gamma >= 1 means progressivity can never dominate deferral' require extra",
    "assumptions about the tax function, the safe-work tax path, the probability",
    "mix, and the limiting sequence being considered. The exact test is always",
    "condition (6).",
    "",
    "In the R model, the practical implementation is:",
    "",
    "1. Choose pf, pm, and ph = 1 - pf - pm.",
    "2. Solve S from the exact pre-tax indifference condition.",
    "3. Compute A(W) and A(S) under the relevant tax treatment.",
    "4. Compute the after-tax CE directly, or equivalently test (6).",
    "",
    "This is why the three-state version is useful: it separates the tax burden",
    "on a moderate same-as-safe outcome from the tax burden on a rare extreme",
    "success outcome.",
    sep = "\n"
  )
)

dev.off()
