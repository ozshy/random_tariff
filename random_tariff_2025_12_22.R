# random_tariff_2025_mm_dd.R 
# Simulations (verification) of computations in a paper titled: "Random Tariff Wards."

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
#library(xtable)# export data frames to LaTeX tables

#Start Figure 1: Business uncertainty survey####

(industry_index.vec = c("A", "B", "C", "D", "E"))
(industry.vec = c("Construction, Real Estate, Mining, Utilities", "Manufacturing", "Retail and Wholesale Trade", "Business Services", "Other Services"))
#
(percentage.vec = round(c(68.5, 86.5, 85.4, 56.1, 60.9)))
#
(sbu.df = data.frame(industry.vec, industry_index.vec, percentage.vec))
#
ggplot(sbu.df, aes(x = reorder(industry_index.vec, percentage.vec), y = percentage.vec)) +
  geom_col(fill="cyan")+ coord_flip() +
  geom_text(aes(label = paste0(percentage.vec, "%")), 
            vjust = 0, hjust = -0.05,
            color = "black",
            size = 6)  +
  geom_text(aes(label = industry.vec), 
            vjust = 0, hjust = 1.5,
            color = "black",
            size = 6) +labs(x="Sector",
                            y = "Percentage answering: YES")  +theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +scale_y_continuous(breaks = seq(0, 100, 10),  labels = function(y) scales::percent(y / 100)) + theme(axis.text.y = element_blank())

#End Figure 1: Business uncertainty survey####

# Model parameters ####

# Model parameters
(alpha = 120)# demand
(beta = 2)
(gamma = 1)
#
(c = 60)# unit production cost
#
#(T = 40)# tariff (specific) => reduce T to obtain lower lambda_hat => improved visualization of the minimum
(T = 44)# tariff (specific) 

### Defining the tariff war probability 
(lambda.vec = seq(0,1,0.005))

#Start Section 4 and Figure 3####

# Verify Assumption 3 ensuring xf = yh > 0 (export levels)
(T_bar = (alpha-c)*(2*beta -gamma)/(2*beta*lambda.vec))
T < T_bar

# Verify assumption that max lambda_min < 1 (Result 2)
(alpha - c)*(2*beta-gamma)^2/(4*beta^2+gamma^2)
T > (alpha - c)*(2*beta-gamma)^2/(4*beta^2+gamma^2)

# predetermined (before realization of war) equilibrium quantities
(xf.vec = (2*T*beta*lambda.vec + (c-alpha)*(2*beta-gamma))/(gamma^2-4*beta^2))
#
(xh.vec = (T*gamma*lambda.vec + (c-alpha)*(gamma-2*beta))/(4*beta^2-gamma^2))
#
(yf.vec = (T*gamma*lambda.vec + (c-alpha)*(gamma-2*beta))/(4*beta^2-gamma^2))
#
(yh.vec = (2*T*beta*lambda.vec +(c-alpha)*(2*beta-gamma))/(gamma^2-4*beta^2))

# predetermined (before realization of war) equilibrium consumer prices 
(pxh.vec = alpha -beta*xh.vec -gamma*yh.vec)
#
(pyh.vec = alpha - gamma*xh.vec - beta*yh.vec)
#
(pxf.vec = alpha - beta*xf.vec-gamma*yf.vec)
#
(pyf.vec = alpha - gamma*xf.vec -beta*yf.vec)

# verify prices are typed correctly in the paper eq (5) => OK
#(pxh_paper.vec = (beta*gamma*lambda.vec*T + (2*beta-gamma)*(alpha*beta +(beta + gamma)*c))/(4*beta^2 -gamma^2))
#
#(pxf_paper.vec = ((2*beta^2-gamma^2)*lambda.vec*T +(2*beta-gamma)*(alpha*beta +(beta+gamma)*c))/(4*beta^2 -gamma^2))

# expected equilibrium seller prices
(sxf.vec = pxf.vec - lambda.vec *T)
#
(syh.vec = pyh.vec - lambda.vec *T)

# expected profits 
(profitx.vec = (pxh.vec-c)*xh.vec +lambda.vec*(pxf.vec-c-T)*xf.vec +(1-lambda.vec)*(pxf.vec-c)*xf.vec)
#
(profity.vec = (pyf.vec-c)*yf.vec +lambda.vec*(pyh.vec-c-T)*yh.vec +(1-lambda.vec)*(pyh.vec-c)*yh.vec)

# find which lambda minimizes profitx 
(lambda_hat=lambda.vec[which.min(profitx.vec)])
# compare with equation (7) in the paper
(lambda_hat = (alpha-c)*(2*beta-gamma)^2/((4*beta^2+gamma^2)*T))

# separating expected profits into domestic and foreign
(profitx_h.vec = (pxh.vec-c)*xh.vec)
#
(profitx_f.vec = lambda.vec*(pxf.vec-c-T)*xf.vec +(1-lambda.vec)*(pxf.vec-c)*xf.vec)

# verify profits are typed correctly in the paper eqs (7-8) => OK
#(profitx_h_paper.vec = (beta*((alpha-c)*(2*beta-gamma) +gamma*lambda.vec*T)^2) / (4*beta^2-gamma^2)^2)
#
#(profitx_f_paper.vec = (beta*((alpha-c)*(2*beta-gamma) -2*beta*gamma*lambda.vec*T)^2) / (4*beta^2-gamma^2)^2)

# make it a data.frame
(profit.df = data.frame(lambda.vec, profitx_h.vec, profitx_f.vec, profitx_h.vec + profitx_f.vec))

ggplot(profit.df, aes(x=lambda.vec)) +geom_line(aes(y=profitx_h.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=profitx_f.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=profitx_h.vec + profitx_f.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,600,50)) +labs(x=TeX("Probability of tariff war: $\\lambda$"), y=TeX("Expected profits of firm  $X$: $E \\pi^X$, $E \\pi_H^X$, $E \\pi_F^X$")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat, color = "black") +annotate("text", x = lambda_hat+0.013, y = 448, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black") +annotate("text", x = 0.40, y = 520, label = TeX("$E \\pi^X=E \\pi^Y$ (total profit)"), size = 8, color="black") +annotate("text", x = 0.20, y = 370, label = TeX("$E \\pi_H^X =E \\pi_F^Y$ (domestic profit)"), size = 8, color="blue") +annotate("text", x = 0.4, y = 200, label = TeX("$E \\pi_F^X =E \\pi_H^Y$ (export profit)"), size = 8, color="red")
#End Figure 3: Profits####

#Start Figure 2 production levels####
(xy.df = data.frame(lambda.vec, xh.vec, xf.vec, xh.vec + xf.vec))

ggplot(xy.df, aes(x=lambda.vec)) +geom_line(aes(y=xh.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=xf.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=xh.vec+xf.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,25,2)) +labs(x=TeX("Probability of tariff war: $\\lambda$"), y=TeX("Domestic sales, export sales, and total sales")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat, color = "black") +annotate("text", x = lambda_hat+0.013, y = 19, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black") +annotate("text", x = 0.40, y = 23, label = TeX("$ x_H + x_F = y_F + y_H$ (total sales)"), size = 8, color="black") +annotate("text", x = 0.20, y = 14, label = TeX("$x_H = y_F$ (domestic sales)"), size = 8, color="blue")  +annotate("text", x = 0.4, y = 10, label = TeX("$x_F = y_H$ (export sales)"), size = 8, color="red")

#End Figure 2 production levels####

#Start Figure 4: prices levels####
T # recall tariff specific rate
c # recall unit cost
T+c # realized cost under tariff war

(pxpy.df = data.frame(lambda.vec, pxh.vec, pxf.vec, sxf.vec))
#
ggplot(pxpy.df, aes(x=lambda.vec)) +geom_line(aes(y=pxh.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=pxf.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=sxf.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(60,110,5)) +labs(x=TeX("Probability of tariff war: $\\lambda$"), y=TeX("Domestic and import consumer price and expected seller price")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat, color = "black") +annotate("text", x = lambda_hat+0.013, y = 70, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black") +annotate("text", x = 0.25, y = 96, label = TeX("$p^X_F=p^Y_H$ (consumer import price)"), size = 8, color="red") +annotate("text", x = 0.3, y = 68, label = TeX("$Es^X_F=Es^Y_H$ (expected seller export price)"), size = 8, color="black") +annotate("text", x = 0.35, y = 83, label = TeX("$p^X_H=p^Y_F$ (domestic price)"), size = 8, color="blue") +geom_hline(yintercept =c, linetype = "F1", size=1.2, color="darkgreen") +geom_hline(yintercept =c+T, linetype = "F1", size=1.2, color="darkgreen") +annotate("text", x = 0.3, y = c+1.8, label = TeX("$c$ (unit cost)"), size = 8, color="darkgreen") +annotate("text", x = 0.3, y = c+T-1.8, label = TeX("$c + T$ (unit cost + specific tariff)"), size = 8, color="darkgreen") +geom_segment(mapping = aes(x=lambda_hat-0.01, y=pxf.vec[which.min(profitx.vec)], xend = lambda_hat-0.01, yend = c+T-0.5), arrow = arrow(ends = "both"), size=1.0, color="darkgreen") +geom_segment(mapping = aes(x=lambda_hat-0.01, y=c+0.5, xend = lambda_hat-0.01, yend = pxf.vec[which.min(profitx.vec)]-0.5), arrow = arrow(ends = "both"), size=1.0, color="darkgreen") +annotate("text", x = lambda_hat-0.04, y = 101, label = TeX("(loss)"), size = 7, color="darkgreen", angle=90) +annotate("text", x = lambda_hat-0.04, y = 78, label = TeX("markup: (gain)"), size = 8, color="darkgreen", angle=90)

#End Figure 4: prices levels####

#Start Section 5: Ad-valorem tariffs, Figure 5####
#(tau = 0.4)# tariff rate => generates figure that is almost identical to Figure 1 (specific tariff) => change it!
(tau = 0.42)

# Verify Assumption 4 (left term)
(alpha-c)*(2*beta-gamma)/(lambda.vec*(c*gamma +alpha*(2*beta-gamma)))
tau < (alpha-c)*(2*beta-gamma)/(lambda.vec*(c*gamma +alpha*(2*beta-gamma)))
#
# Verify Assumption 4 (right term) => deleted, implied by the above!
(alpha-c)*(2*beta-gamma)/(lambda.vec*(-2*beta*c +alpha*(2*beta-gamma)))
tau < (alpha-c)*(2*beta-gamma)/(lambda.vec*(-2*beta*c +alpha*(2*beta-gamma)))

# equilibrium sales (ad-valorem tariff)
(xf_ad.vec = (c*(2*beta + gamma*(lambda.vec*tau -1)) +alpha*(2*beta-gamma) *(lambda.vec*tau-1))/((4*beta^2 -gamma^2)*(lambda.vec*tau -1)))
#
(xh_ad.vec = (c*(2*beta*(lambda.vec*tau -1) +gamma) +alpha*(1-lambda.vec*tau)*(2*beta-gamma))/((1-lambda.vec*tau)*(4*beta^2-gamma^2)))
#
(yf_ad.vec = (c*(2*beta*(lambda.vec*tau -1) +gamma) +alpha*(1-lambda.vec*tau) *(2*beta-gamma))/((1-lambda.vec*tau) *(4*beta^2-gamma^2)))
#
(yh_ad.vec = (c*(2*beta +gamma*(lambda.vec*tau -1)) +alpha*((2*beta-gamma) *(lambda.vec*tau -1)))/((4*beta^2-gamma^2)*(lambda.vec*tau -1)))


# equilibrium prices (ad-valorem tariff)
(pxh_ad.vec = alpha -beta*xh_ad.vec -gamma*yh_ad.vec)
#
(pyh_ad.vec = alpha - gamma*xh_ad.vec - beta*yh_ad.vec)
#
(pxf_ad.vec = alpha - beta*xf_ad.vec-gamma*yf_ad.vec)
#
(pyf_ad.vec = alpha - gamma*xf_ad.vec -beta*yf_ad.vec)

# verify prices are typed correctly in the paper eqs (14-15) => OK
#(pxh_ad_paper.vec = (c*(2*beta^2 +beta*gamma -gamma^2*(1-lambda.vec*tau)) +alpha*beta*(2*beta-gamma)*(1-lambda.vec*tau)) /((4*beta^2 -gamma^2)*(1-lambda.vec*tau)))
#
#(pxf_ad_paper.vec = (c*(2*beta^2 +beta*gamma*(1-lambda.vec*tau) -gamma^2) +alpha*beta*(2*beta-gamma)*(1-lambda.vec*tau)) /((4*beta^2 -gamma^2)*(1-lambda.vec*tau)))


# expected equilibrium seller prices
(sxf_ad.vec = pxf_ad.vec*(1-lambda.vec *tau))
#
(syh_ad.vec = pyh_ad.vec*(1-lambda.vec *tau))

# expected profits under ad valorem
(profitx_h_ad.vec = (pxh_ad.vec-c)*xh_ad.vec)
#
(profitx_f_ad.vec = lambda.vec*((1-tau)*pxf_ad.vec-c)*xf_ad.vec +(1-lambda.vec)*(pxf_ad.vec-c)*xf_ad.vec)

# verify profits are typed correctly in the paper eqs (16-17) => OK
#(profit_h_paper.vec = beta*(c*(gamma-2*beta*(1-lambda.vec*tau)) +alpha*(2*beta -gamma) *(1-lambda.vec*tau))^2/((4*beta^2 -gamma^2)^2 *(1-lambda.vec*tau)^2))
#
#(profit_f_paper.vec = beta*(c*(2*beta -gamma*(1-lambda.vec*tau)) -alpha*(2*beta -gamma) *(1-lambda.vec*tau))^2/((4*beta^2 -gamma^2)^2 *(1-lambda.vec*tau)))

# find which lambda minimizes profitx 
which.min(profitx_h_ad.vec + profitx_f_ad.vec)
(lambda_hat_ad=lambda.vec[which.min(profitx_h_ad.vec + profitx_f_ad.vec)])

# make it a data.frame
(profit_ad.df = data.frame(lambda.vec, profitx_h_ad.vec, profitx_f_ad.vec, profitx_h_ad.vec + profitx_f_ad.vec))

ggplot(profit_ad.df, aes(x=lambda.vec)) +geom_line(aes(y=profitx_h_ad.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=profitx_f_ad.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=profitx_h_ad.vec + profitx_f_ad.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,600,50)) +labs(x=TeX("Probability of ad-valorem tariff war: $\\lambda$"), y=TeX("Expected profits of firm  $X$: $E \\pi^X$, $E \\pi_H^X$, $E \\pi_F^X$")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat_ad, color = "black") +annotate("text", x = lambda_hat_ad+0.017, y = 445, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black") +annotate("text", x = 0.40, y = 525, label = TeX("$E \\pi^X=E \\pi^Y$ (total profit)"), size = 8, color="black") +annotate("text", x = 0.25, y = 355, label = TeX("$E \\pi_H^X =E \\pi_F^Y$ (domestic profit)"), size = 8, color="blue") +annotate("text", x = 0.40, y = 215, label = TeX("$E \\pi_F^X =E \\pi_H^Y$ (export profit)"), size = 8, color="red")

#End Figure 5: ad-valorem, end of Section 5####

# #Start Section 6 (deleted): Unilateral tariff (H only), just simulations, not in the paper####
# # need to increase T to satisfy the condition below
# (T_ind = 44)
# #
# # Verify Assumption 3 ensuring yh_ind > 0 (export levels)
# (alpha-c)*(2*beta -gamma)/(2*beta*lambda.vec)
# T_ind < (alpha-c)*(2*beta -gamma)/(2*beta*lambda.vec)
# #
# # Verify eq (F.2): profit y declines with lambda
# T_ind*gamma*lambda.vec - (alpha-c)*(2*beta-gamma)
# 
# (xf_ind.vec = (alpha-c)/(2*beta+gamma))
# #
# (xh_ind.vec = (T_ind*gamma*lambda.vec+(c-alpha)*(gamma-2*beta))/(4*beta^2-gamma^2))
# #
# (yf_ind.vec = xf_ind.vec)
# #
# (yh_ind.vec = (2*T_ind*beta*lambda.vec+(c-alpha)*(2*beta-gamma))/(gamma^2-4*beta^2))
# 
# (pxh_ind.vec = alpha -beta*xh_ind.vec -gamma*yh_ind.vec)
# #
# (pyh_ind.vec = alpha - gamma*xh_ind.vec - beta*yh_ind.vec)
# #
# (pxf_ind.vec = alpha - beta*xf_ind.vec-gamma*yf_ind.vec)
# #
# (pyf_ind.vec = alpha - gamma*xf_ind.vec -beta*yf_ind.vec)
# 
# # verify prices are typed correctly in the paper eqs (21) => OK
# #(pxh_ind_paper.vec = (T_ind*beta*gamma*lambda.vec +(2*beta-gamma) *(alpha*beta +c*(beta+gamma))) /(4*beta^2-gamma^2))
# #
# #(pyh_ind_paper.vec = (T_ind*lambda.vec*(2*beta^2-gamma^2) +(2*beta-gamma) *(alpha*beta +c*(beta+gamma))) /(4*beta^2-gamma^2))
# #
# #(pxf_ind_paper.vec = (alpha*beta +c*(beta+gamma))/(2*beta+gamma))
# 
# 
# (profitx_ind.vec = (pxh_ind.vec-c)*xh_ind.vec +(pxf_ind.vec-c)*xf_ind.vec)
# #
# (profity_ind.vec = (pyf_ind.vec-c)*yf_ind.vec +lambda.vec*(pyh_ind.vec-c-T_ind)*yh_ind.vec +(1-lambda.vec)*(pyh_ind.vec-c)*yh_ind.vec)
# 
# # verify profits are typed correctly in the paper eqs (22-23) => OK
# #(profitx_ind_paper.vec = beta*(T_ind^2*gamma^2*lambda.vec^2 +2*T_ind*gamma*lambda.vec *(alpha-c)*(2*beta-gamma) +2*(alpha-c)^2 *(2*beta-gamma)^2) /((2*beta+gamma)^2*(2*beta-gamma)^2))
# #
# #(profity_ind_paper.vec = 2*beta*(2*T_ind^2*beta^2*lambda.vec^2 -2*T_ind*beta*lambda.vec *(alpha-c)*(2*beta-gamma) +(alpha-c)^2 *(2*beta-gamma)^2) /((2*beta+gamma)^2*(2*beta-gamma)^2))
# 
# #verify monotonicity w.r.t. lambda.vec [Result 4 in the paper]
# lambda.vec[which.max(profitx_ind.vec)]
# lambda.vec[which.min(profitx_ind.vec)]
# #
# lambda.vec[which.max(profity_ind.vec)]
# lambda.vec[which.min(profity_ind.vec)]
# 
# #End Section 6 (deleted): Unilateral tariff (H only), just simulations, not in the paper####
