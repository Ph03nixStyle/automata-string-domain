import matplotlib.pyplot as plt
import numpy as np

lst1 = np.loadtxt("etats_sans_minimisation.txt")
lst2 = np.loadtxt("etats_avec_minimisation.txt")
X1 = np.array([i for i in range(len(lst1))])
X2 = np.array([i/2 for i in range(len(lst2))])

plt.plot(X1, lst1, color='r', label='sans minimisation')
plt.plot(X2, lst2, color='g', label='avec minimisation')

plt.xlabel("Nombre de if")
plt.ylabel("Nombre d'états")
# plt.title("Evolution du nombre d'états")
plt.legend()

plt.show()