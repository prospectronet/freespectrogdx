package priv.sp.house

import priv.sp._

class DudeMancer {

  val Dude = House("dudes", List(
    new Creature("dudes.Bred", Attack(5), 17),
    new Creature("dudes.TwoP", Attack(4), 29),
    new Creature("dudes.Axl", Attack(5), 31),
    new Creature("dudes.BillBull", Attack(6), 32),
    new Creature("dudes.ElGado", Attack(7), 40),
    new Creature("dudes.Andore", Attack(8), 50),
    new Creature("dudes.Rolento", Attack(9), 65),
    new Creature("dudes.EdiE", Attack(6), 99)))

  Dude.initCards(Houses.basicCostFunc)
}
