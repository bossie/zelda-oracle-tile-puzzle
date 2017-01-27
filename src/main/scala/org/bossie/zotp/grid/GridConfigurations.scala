package org.bossie.zotp.grid

object GridConfigurations {

  // http://faqsmedia.ign.com/faqs/image/zelda_oracle_of_ages_tile_3.gif
  val ooa3 = GridConfiguration.fromString(
    """..#......
      |..#....#.
      |.........
      |.#..#...#
      |...#.....
      |.......#.
      |.s#......""".stripMargin)

  // with an extra row at the bottom
  val ooa3Extended = GridConfiguration.fromString(
    """..#......
      |..#....#.
      |.........
      |.#..#...#
      |...#.....
      |.......#.
      |.s#......
      |.........""".stripMargin)

  // http://faqsmedia.ign.com/faqs/image/zelda_oracle_of_ages_tile_4.gif
  val ooa2 = GridConfiguration.fromString(
    """.............
      |.##..#.......
      |.......#..#..
      |#............
      |#...........s
      |......#......
      |...........#.
      |...#....#..#.
      |.............""".stripMargin)
}
