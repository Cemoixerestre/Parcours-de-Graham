"""Implémentation du parcours de Graham en python 3.
Contient les définitions nécessaire au fonctionnement de l'algorithme, la fonction enveloppe_convexe et un affichage graphique
avec Tkinter pour tester de manière interactive ladite fonction."""

class Vecteur:
    """Représentation de vecteurs en 2 dimensions. Permet de comparer entre deux vecteurs, lequel est le plus à droite."""
    def __init__(self, point_a, point_b):
        """Nouveau vecteur en fonction de 2 couples de coordonnées de points."""
        self.x = point_b[0] - point_a[0]
        self.y = point_b[1] - point_a[1]

    def det(self, other):
        """Déterminant de deux vecteurs."""
        return self.x * other.y - self.y * other.x

    def normcarr(self):
        """Le carré de la norme de self."""
        return self.x ** 2 + self.y ** 2

    def __lt__(self, other):
        """self < other : renvoie True si le vecteur self est « plus à
        gauche » que le vecteur other ou que si les deux sont colinéaires,
        si self a une norme plus petite que other."""
        det = self.det(other)
        if det < 0:
            return True
        if det > 0:
            return False
        return self.normcarr() < other.normcarr()
    
def enveloppe_convexe(points):
    """Renvoie la liste des sommets de l'enveloppe convexe d'une liste de
    points du plan, en utilisant le parcours de Graham."""
    if len(points) <= 3:
        return points.copy()

    #on recherche le pivot, c'est-à-dire le point de plus petite abscisse et de
    #plus petite ordonnée à défaut.
    pivot = points[0]
    for p in points:
        if p[0] < pivot[0] or (p[0] == pivot[0] and p[1] < pivot[1]):
            pivot = p

    points.sort(key=lambda p: Vecteur(pivot, p))
    #Tri des points en fonction de leur angle.

    pile = [pivot, points[1]]
    for i in range(2, len(points)):
        p = points[i]
        while len(pile) >= 2 and \
              Vecteur(pile[-2], p).det(Vecteur(pile[-2], pile[-1])) <= 0:
                  #Si les points pile[-2], pile[-1], p forment un tour à droite :
            pile.pop()
        pile.append(p)
 
    return pile

#Test avec Tkinter :
if __name__ == "__main__":
    import tkinter as tk
    from itertools import chain
    fenetre = tk.Tk()
    
    c = tk.Canvas(fenetre, height=800, width=800, bg="white")
    c.pack()

    points = []
    enveloppe = []
    num = None
    
    def ajouter_point(event):
        global num, enveloppe
        points.append((event.x, event.y))
        c.create_oval(event.x-3, event.y-3, event.x+3, event.y+3, fill="black")
        enveloppe = enveloppe_convexe(points)
        if num is None:
            num = c.create_polygon(*chain(*enveloppe), fill="white",
                                   outline="black", width=1.5)
        else:
            c.coords(num, *chain(*enveloppe))

    c.bind("<Button-1>", ajouter_point)

    def reset():
        global num, points, enveloppe
        c.delete(tk.ALL)
        points = []
        enveloppe = []
        num = None
        
    b = tk.Button(fenetre, text="reset", command=reset)
    b.pack()

    fenetre.mainloop()
