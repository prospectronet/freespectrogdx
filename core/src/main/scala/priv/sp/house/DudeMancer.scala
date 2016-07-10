package priv.sp.house

import priv.sp._

class DudeMancer {

  val Dude = House("dudes", List(
    new Creature("dudes.Bred", Attack(4), 17),
    new Creature("dudes.TwoP", Attack(4), 27),
    new Creature("dudes.Axl", Attack(5), 26),
    new Creature("dudes.BillBull", Attack(7), 31),
    new Creature("dudes.ElGado", Attack(8), 46),
    new Creature("dudes.Andore", Attack(9), 52),
    new Creature("dudes.Rolento", Attack(10), 65),
    new Creature("dudes.EdiE", Attack(10), 99)))

  Dude.initCards(Houses.basicCostFunc)
}
