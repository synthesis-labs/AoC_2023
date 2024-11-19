using System.Numerics;
using NetTopologySuite.Geometries;

namespace AOC2023
{
    internal class Day24
    {
        String[] lines = File.ReadAllLines("input.day24.txt");
        double minRange = 200000000000000;
        double maxRange = 400000000000000;
        //double minRange = 7;
        //double maxRange = 27;
        List<Hailstone> hailstones = new List<Hailstone>();
        int counter = 0;

        internal void Execute()
        {
            EnumerateAllLines();
            IterateHailstones();

            Console.WriteLine("Day 24: " + counter);
        }

        private void EnumerateAllLines()
        {
            foreach (var line in lines) 
            {
                var positionVelocitySplit = line.Replace(" ", "").Split('@');
                var positions = positionVelocitySplit[0].Split(',');
                var velocities = positionVelocitySplit[1].Split(",");

                hailstones.Add(new Hailstone() 
                {
                    px = Double.Parse(positions[0]),
                    py = Double.Parse(positions[1]),
                    pz = Double.Parse(positions[2]),
                    vx = Int32.Parse(velocities[0]),
                    vy = Int32.Parse(velocities[1]),
                    vz = Int32.Parse(velocities[2])
                });
            }
        }

        private void IterateHailstones()
        {
            for (int i = 0; i < hailstones.Count; i++) 
            {
                var hailstoneOne = hailstones[i];

                for (int j = i + 1; j < hailstones.Count; j++) 
                {
                    var hailstoneTwo = hailstones[j];
                    if (DetermineIntersection(hailstoneOne, hailstoneTwo))
                        counter++;
                }
            }
        }

        private bool DetermineIntersection(Hailstone one, Hailstone two)
        {
            var hailstoneOne = new Hailstone() { px = one.px, py = one.py, pz = one.pz, vx = one.vx, vy = one.vy, vz = one.vz };
            var hailstoneTwo = new Hailstone() { px = two.px, py = two.py, pz = two.pz, vx = two.vx, vy = two.vy, vz = two.vz };

            LineString line1 = new LineString(new[]
            {
                new Coordinate(hailstoneOne.px, hailstoneOne.py),
                new Coordinate(hailstoneOne.px, hailstoneOne.py)
            });

            LineString line2 = new LineString(new[]
            {
                new Coordinate(hailstoneTwo.px, hailstoneTwo.py),
                new Coordinate(hailstoneTwo.px, hailstoneTwo.py)
            });

            while (true)
            {
                if (hailstoneOne.px >= minRange && hailstoneOne.px <= maxRange && 
                    hailstoneOne.py >= minRange && hailstoneOne.py <= maxRange &&
                    hailstoneTwo.px >= minRange && hailstoneTwo.px <= maxRange &&
                    hailstoneTwo.py >= minRange && hailstoneTwo.py <= maxRange)
                {
                    hailstoneOne.px += hailstoneOne.vx;
                    hailstoneOne.py += hailstoneOne.vy;

                    hailstoneTwo.px += hailstoneTwo.vx;
                    hailstoneTwo.py += hailstoneTwo.vy;
                }
                else
                {
                    line1 = new LineString(line1.Coordinates.Append(new Coordinate(hailstoneOne.px, hailstoneOne.py)).ToArray());
                    line2 = new LineString(line2.Coordinates.Append(new Coordinate(hailstoneTwo.px, hailstoneTwo.py)).ToArray());

                    return line1.Intersects(line2);
                }
            }
        }
    }

    internal class Hailstone
    {
        //px py pz @ vx vy vz
        internal double px { get; set; }
        internal double py { get; set; }
        internal double pz { get; set; }
        internal int vx { get; set; }
        internal int vy { get; set; }
        internal int vz { get; set; }
    }
}
