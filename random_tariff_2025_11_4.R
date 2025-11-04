# random_tariff_2025_mm_dd.R 
# Simulations (verification) of computations in a paper titled: "Random Tariff Wards."

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
#library(xtable)# export data frames to LaTeX tables

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

# Verify Assumption 3 ensuring xf = yh > 0 (export levels)
(alpha-c)*(2*beta -gamma)/(2*beta*lambda.vec)
T < (alpha-c)*(2*beta -gamma)/(2*beta*lambda.vec)

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

# make it a data.frame
(profit.df = data.frame(lambda.vec, profitx_h.vec, profitx_f.vec, profitx_h.vec + profitx_f.vec))

ggplot(profit.df, aes(x=lambda.vec)) +geom_line(aes(y=profitx_h.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=profitx_f.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=profitx_h.vec + profitx_f.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,600,50)) +labs(x=TeX("Probability of tariff war: $\\lambda$"), y=TeX("Expected profits of firm  $X$: $E \\pi^X$, $E \\pi_H^X$, $E \\pi_F^X$")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat, color = "black") +annotate("text", x = lambda_hat+0.013, y = 448, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black") +annotate("text", x = 0.3, y = 520, label = TeX("$E \\pi^X=E \\pi^Y$"), size = 8, color="black") +annotate("text", x = 0.3, y = 370, label = TeX("$E \\pi_H^X =E \\pi_F^Y$"), size = 8, color="blue") +annotate("text", x = 0.3, y = 200, label = TeX("$E \\pi_F^X =E \\pi_H^Y$"), size = 8, color="red")
#End Figure 1####

#Start Figure production levels (not in the paper)####
(xy.df = data.frame(lambda.vec, xh.vec, xf.vec, xh.vec + xf.vec))

ggplot(xy.df, aes(x=lambda.vec)) +geom_line(aes(y=xh.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=xf.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=xh.vec+xf.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,25,2)) +labs(x=TeX("Probability of reciprocal import tariffs: $\\lambda$"), y=TeX("Domestic and export sales of firm  $X$: $x_H$, $x_F$, ($x_H+x_F$)")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat, color = "black") +annotate("text", x = lambda_hat+0.013, y = 19, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black")

#End Figure production levels (not in the paper)####

#Start Figure prices (not in the paper)####
(pxpy.df = data.frame(lambda.vec, pxh.vec, pxf.vec, sxf.vec))

ggplot(pxpy.df, aes(x=lambda.vec)) +geom_line(aes(y=pxh.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=pxf.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=sxf.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(60,110,5)) +labs(x=TeX("Probability of reciprocal import tariffs: $\\lambda$"), y=TeX("Consumer and producer prices of $X$: $p^X_H$, $p^X_F$, $r^X_H$")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat, color = "black") +annotate("text", x = lambda_hat+0.013, y = 80, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black")

#End Figure prices levels (not in the paper)####

#Start Section 4: Ad-valorem tariffs, Figure 2####
#(tau = 0.4)# tariff rate => generates figure that is almost identical to Figure 1 (specific tariff) => change it!
(tau = 0.42)

# Verify Assumption 4 (left term)
(alpha-c)*(2*beta-gamma)/(lambda.vec*(c*gamma +alpha*(2*beta-gamma)))
tau < (alpha-c)*(2*beta-gamma)/(lambda.vec*(c*gamma +alpha*(2*beta-gamma)))
#
# Verify Assumption 4 (right term) => deleted, implied by the above!
(alpha-c)*(2*beta-gamma)/(lambda.vec*(-2*beta*c +alpha*(2*beta-gamma)))
tau < (alpha-c)*(2*beta-gamma)/(lambda.vec*(-2*beta*c +alpha*(2*beta-gamma)))


(xf_ad.vec = (c*(2*beta + gamma*(lambda.vec*tau -1)) +alpha*(2*beta-gamma) *(lambda.vec*tau-1))/((4*beta^2 -gamma^2)*(lambda.vec*tau -1)))
#
(xh_ad.vec = (c*(2*beta*(lambda.vec*tau -1) +gamma) +alpha*(1-lambda.vec*tau)*(2*beta-gamma))/((1-lambda.vec*tau)*(4*beta^2-gamma^2)))
#
(yf_ad.vec = (c*(2*beta*(lambda.vec*tau -1) +gamma) +alpha*(1-lambda.vec*tau) *(2*beta-gamma))/((1-lambda.vec*tau) *(4*beta^2-gamma^2)))
#
(yh_ad.vec = (c*(2*beta +gamma*(lambda.vec*tau -1)) +alpha*((2*beta-gamma) *(lambda.vec*tau -1)))/((4*beta^2-gamma^2)*(lambda.vec*tau -1)))

(pxh_ad.vec = alpha -beta*xh_ad.vec -gamma*yh_ad.vec)
#
(pyh_ad.vec = alpha - gamma*xh_ad.vec - beta*yh_ad.vec)
#
(pxf_ad.vec = alpha - beta*xf_ad.vec-gamma*yf_ad.vec)
#
(pyf_ad.vec = alpha - gamma*xf_ad.vec -beta*yf_ad.vec)

# expected equilibrium seller prices
(sxf_ad.vec = pxf_ad.vec*(1-lambda.vec *tau))
#
(syh_ad.vec = pyh_ad.vec*(1-lambda.vec *tau))

# expected profits under ad valorem
(profitx_h_ad.vec = (pxh_ad.vec-c)*xh_ad.vec)
#
(profitx_f_ad.vec = lambda.vec*((1-tau)*pxf_ad.vec-c)*xf_ad.vec +(1-lambda.vec)*(pxf_ad.vec-c)*xf_ad.vec)

# find which lambda minimizes profitx 
which.min(profitx_h_ad.vec + profitx_f_ad.vec)
(lambda_hat_ad=lambda.vec[which.min(profitx_h_ad.vec + profitx_f_ad.vec)])


# make it a data.frame
(profit_ad.df = data.frame(lambda.vec, profitx_h_ad.vec, profitx_f_ad.vec, profitx_h_ad.vec + profitx_f_ad.vec))

ggplot(profit_ad.df, aes(x=lambda.vec)) +geom_line(aes(y=profitx_h_ad.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=profitx_f_ad.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=profitx_h_ad.vec + profitx_f_ad.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,600,50)) +labs(x=TeX("Probability of ad-valorem tariff war: $\\lambda$"), y=TeX("Expected profits of firm  $X$: $E \\pi^X$, $E \\pi_H^X$, $E \\pi_F^X$")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat_ad, color = "black") +annotate("text", x = lambda_hat_ad+0.017, y = 445, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black") +annotate("text", x = 0.3, y = 525, label = TeX("$E \\pi^X=E \\pi^Y$"), size = 8, color="black") +annotate("text", x = 0.3, y = 355, label = TeX("$E \\pi_H^X =E \\pi_F^Y$"), size = 8, color="blue") +annotate("text", x = 0.3, y = 215, label = TeX("$E \\pi_F^X =E \\pi_H^Y$"), size = 8, color="red")
#End Figure 2####


#Start Section 5: Unilateral tariff (H only), just simulations, not in the paper####
# need to increase T to satisfy the condition below
(T_ind = 50)
# the condition for having a minimum lambda_y for min profit y
((c-alpha)*(gamma-2*beta)/(2*beta))
T_ind > ((c-alpha)*(gamma-2*beta)/(2*beta))

(xf_ind.vec = (alpha-c)/(2*beta+gamma))
(xh_ind.vec = (T_ind*gamma*lambda.vec+(c-alpha)*(gamma-2*beta))/(4*beta^2-gamma^2))
(yf_ind.vec = xf_ind.vec)
(yh_ind.vec = (2*T_ind*beta*lambda.vec+(c-alpha)*(2*beta-gamma))/(gamma^2-4*beta^2))

(pxh_ind.vec = alpha -beta*xh_ind.vec -gamma*yh_ind.vec)
#
(pyh_ind.vec = alpha - gamma*xh_ind.vec - beta*yh_ind.vec)
#
(pxf_ind.vec = alpha - beta*xf_ind.vec-gamma*yf_ind.vec)
#
(pyf_ind.vec = alpha - gamma*xf_ind.vec -beta*yf_ind.vec)


(profitx_ind.vec = (pxh.vec-c)*xh_ind.vec +(pxf.vec-c)*xf_ind.vec)
#
(profity_ind.vec = (pyf_ind.vec-c)*yf_ind.vec +lambda.vec*(pyh_ind.vec-c-T_ind)*yh_ind.vec +(1-lambda.vec)*(pyh_ind.vec-c)*yh_ind.vec)

 #End Section 5: Unilateral tariff (H only),just simulations, not in the paper####
