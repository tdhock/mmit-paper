"""
Figures that show the differences between the Interval-CART and MMIT loss functions

"""
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns; sns.set_style("white")

mmit_color = "red"
cart_color = "blue"
cart_partial_color = "lightblue"

def mmit_loss(y, x, margin=0., lower=True):
    if lower:
        s = -1
    else:
        s = 1
    v = s * (x - y) + margin
    v[v <= 0] = 0.
    v = v**2

    b = 1.0 * y - s * margin

    return v, b

def cart_loss(y, x, margin=0., lower=True):
    if lower:
        s = -1
    else:
        s = 1
    v = s * (x - y) + margin
    v = v**2

    b = 1.0 * y - s * margin

    return v, b


# First figure shows the difference between the loss functions of Interval-CART and MMIT
f, [ax1, ax2, ax3] = plt.subplots(nrows=1, ncols=3, sharex=True, sharey=True)

f.set_size_inches(w=7.5, h=3)

x = np.linspace(0, 10, 1000)
lower_bound = 4.0
upper_bound = 6.0

# Interval-censored case
y_lower = mmit_loss(lower_bound, x, lower=True)[0]
y_upper = mmit_loss(upper_bound, x, lower=False)[0]
ax1.plot(x, y_lower + y_upper, color=mmit_color, label="MMIT", zorder=10, linewidth=1.5)
#
y_lower_cart = cart_loss(lower_bound, x, lower=True)[0]
y_upper_cart = cart_loss(upper_bound, x, lower=False)[0]
ax1.plot(x, y_lower_cart, linestyle="-.", label="CART (Lower limit)", color=cart_partial_color)
ax1.plot(x, y_upper_cart, linestyle="-.", color=cart_partial_color, label="CART (Upper limit)")
ax1.plot(x, y_lower_cart + y_upper_cart, color=cart_color, label="CART (Total loss)", linewidth=1.5)
#
ax1.scatter([lower_bound], [0.], edgecolor="black", facecolor="black", linewidth=1.0, zorder=100., label="Lower limit")
ax1.scatter([upper_bound], [0.], edgecolor="black", facecolor="none", linewidth=1.0, zorder=100., label="Upper limit")
ax1.plot([lower_bound, upper_bound], [0., 0.], linestyle="--", color="black", label="Interval", zorder=20)
#
ax1.set_xlabel("$\mu$")
ax1.set_ylabel("Loss")


# Left-censored case
y_upper = mmit_loss(upper_bound, x, lower=False)[0]
ax2.plot(x, y_upper, color=mmit_color, label="MMIT", zorder=10, linewidth=1.5)
#
y_upper_cart = cart_loss(upper_bound, x, lower=False)[0]
ax2.plot(x, y_upper_cart, color=cart_color, label="CART (Upper limit)", linewidth=1.5)
#
ax2.scatter([upper_bound], [0.], edgecolor="black", facecolor="none", linewidth=1.0, zorder=100., label="Upper limit")
ax2.plot([x.min(), upper_bound], [0., 0.], linestyle="--", color="black", label="Interval", zorder=20)
#
ax2.set_xlabel("$\mu$")
ax2.set_ylabel("Loss")


# Right-censored case
y_lower = mmit_loss(lower_bound, x, lower=True)[0]
ax3.plot(x, y_lower, color=mmit_color, label="MMIT", zorder=10, linewidth=1.5)
#
y_lower_cart = cart_loss(lower_bound, x, lower=True)[0]
ax3.plot(x, y_lower_cart, color=cart_color, label="CART (Lower limit)", linewidth=1.5)
#
ax3.scatter([lower_bound], [0.], edgecolor="black", facecolor="black", linewidth=1.0, zorder=100., label="Upper limit")
ax3.plot([lower_bound, x.max()], [0., 0.], linestyle="--", color="black", label="Interval", zorder=20)
#
ax3.set_xlabel("$\mu$")
ax3.set_ylabel("Loss")

#

ax1.set_xlim([2.0, 8.0])
ax1.set_ylim([-0.2, 8.0])

ax1.legend(ncol=1, bbox_to_anchor=(4.5, 1.0))  # Legend here since the first subplot has the most elements

# plt.tight_layout()
# plt.show()
plt.savefig("figure.cart.vs.mmit.loss.pdf", bbox_inches="tight")





# Second figure: Interval-CART sticks to interval limits in the left/right-censored cases
plt.clf()

true_target = 3.0

f, [ax1, ax2, ax3, ax4] = plt.subplots(ncols=4, sharex=True, sharey=True)
f.set_size_inches(w=9.5, h=2.5)

# Case 1: no left-censored
y_lower = mmit_loss(true_target - 0.1, x, lower=True)[0]
y_upper = mmit_loss(true_target + 0.1, x, lower=False)[0]
ax1.plot(x, y_lower + y_upper, color=mmit_color)
y_lower_cart = cart_loss(true_target - 0.1, x, lower=True)[0]
y_upper_cart = cart_loss(true_target + 0.1, x, lower=False)[0]
ax1.plot(x, y_lower_cart + y_upper_cart, color=cart_color)
ax1.axvline(true_target, color="black", linestyle="--")
ax1.set_title("a) No left-censoring")

# Case 2: close left censored
censored_interval_upper_bound = true_target + 1.0
y_lower = mmit_loss(true_target - 0.1, x, lower=True)[0]
y_upper = mmit_loss(true_target + 0.1, x, lower=False)[0] + mmit_loss(censored_interval_upper_bound, x, lower=False)[0]
ax2.plot(x, y_lower + y_upper, color=mmit_color)
y_lower_cart = cart_loss(true_target - 0.1, x, lower=True)[0]
y_upper_cart = cart_loss(true_target + 0.1, x, lower=False)[0] + cart_loss(censored_interval_upper_bound, x, lower=False)[0]
ax2.plot(x, y_lower_cart + y_upper_cart, color=cart_color)
ax2.axvline(true_target, color="black", linestyle="--")
ax2.set_title("b) Left-censoring at {}".format(censored_interval_upper_bound))

# Case 3: close left censored
censored_interval_upper_bound = true_target + 3.0
y_lower = mmit_loss(true_target - 0.1, x, lower=True)[0]
y_upper = mmit_loss(true_target + 0.1, x, lower=False)[0] + mmit_loss(censored_interval_upper_bound, x, lower=False)[0]
ax3.plot(x, y_lower + y_upper, color=mmit_color)
y_lower_cart = cart_loss(true_target - 0.1, x, lower=True)[0]
y_upper_cart = cart_loss(true_target + 0.1, x, lower=False)[0] + cart_loss(censored_interval_upper_bound, x, lower=False)[0]
ax3.plot(x, y_lower_cart + y_upper_cart, color=cart_color)
ax3.axvline(true_target, color="black", linestyle="--")
ax3.set_title("c) Left-censoring at {}".format(censored_interval_upper_bound))

# Case 4: close left censored
censored_interval_upper_bound = true_target + 5.0
y_lower = mmit_loss(true_target - 0.1, x, lower=True)[0]
y_upper = mmit_loss(true_target + 0.1, x, lower=False)[0] + mmit_loss(censored_interval_upper_bound, x, lower=False)[0]
ax4.plot(x, y_lower + y_upper, color=mmit_color, label="MMIT")
y_lower_cart = cart_loss(true_target - 0.1, x, lower=True)[0]
y_upper_cart = cart_loss(true_target + 0.1, x, lower=False)[0] + cart_loss(censored_interval_upper_bound, x, lower=False)[0]
ax4.plot(x, y_lower_cart + y_upper_cart, color=cart_color, label="CART")
ax4.axvline(true_target, color="black", linestyle="--", label="True target")
ax4.set_title("d) Left-censoring at {}".format(censored_interval_upper_bound))

ax4.set_xlim([0, 8])
ax4.set_ylim([-0.3, 30])

ax1.set_xlabel("$\mu$")
ax2.set_xlabel("$\mu$")
ax3.set_xlabel("$\mu$")
ax4.set_xlabel("$\mu$")

ax1.set_ylabel("Loss")
ax2.set_ylabel("Loss")
ax3.set_ylabel("Loss")
ax4.set_ylabel("Loss")

plt.legend(bbox_to_anchor=(0.0, 1.28), ncol=3)
plt.savefig("interval.cart.fails.with.open.intervals.pdf", bbox_inches="tight")
