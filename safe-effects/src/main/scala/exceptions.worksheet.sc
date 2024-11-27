import exceptions.*

val l1 = List('d', 's', 'h', 'q', 'o', 'z', 'i')

l1.containsExn('t')
l1.containsExn('s')
l1.containsExn('d')

l1.containsBoundary('h')
l1.containsBoundary('c')
l1.containsBoundary('q')


l1.findExn(_.toInt == 115)
l1.findExn(_ > 'u')
l1.findExn(_ < 'c')

l1.findBoundary(_.toInt == 115)
l1.findBoundary(_ > 'u')
l1.findBoundary(_ < 'c')