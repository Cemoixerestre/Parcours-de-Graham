class Vecteur:
    def __init__(self, point_a, point_b):
        self.x = point_b[0] - point_a[0]
        self.y = point_b[1] - point_a[1]

    def det(self, other):
        return self.x * other.y - self.y * other.x

    def normcarr(self):
        """Le carré de la norme de self."""
        return self.x ** 2 + self.y ** 2


class Vecteur1(Vecteur):
    def __lt__(self, other):
        """self < other : renvoie True si le vecteur self est « plus à
        gauche » que le vecteur other."""
        det = self.det(other)
        if det < 0:
            return True
        if det > 0:
            return False
        return self.normcarr() < other.normcarr()

class Vecteur2(Vecteur):
    def __lt__(self, other):
        """self < other : renvoie True si le vecteur self est « plus à
        gauche » que le vecteur other."""
        det = self.det(other)
        if det < 0:
            return True
        if det > 0:
            return False
        return self.normcarr() > other.normcarr()
    
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

    points.sort(key=lambda p: Vecteur1(pivot, p))

    pile = [pivot, points[1]]
    for i in range(2, len(points)):
        p = points[i]
        while len(pile) >= 2 and \
              Vecteur(pile[-2], p).det(Vecteur(pile[-2], pile[-1])) <= 0:
            pile.pop()
        pile.append(p)
 
    return pile

def jarvis(points):
    if len(points) <= 3:
        return points

    pivot = points[0]
    for p in points:
        if p[0] < pivot[0] or (p[0] == pivot[0] and p[1] < pivot[1]):
            pivot = p

    enveloppe = [pivot]
    suivant = min(points, key=lambda p: Vecteur2(pivot, p))
    
    while suivant != pivot:
        enveloppe.append(suivant)
        suivant = min(points, key=lambda p: Vecteur2(suivant, p))
        
    return enveloppe
    

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
        enveloppe = jarvis(points)
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
