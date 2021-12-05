var testInput =
@"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2".Split("\n");

var input = await System.IO.File.ReadAllLinesAsync("input.txt");

Console.WriteLine($"part1 test: {SolvePart1(ParseLines(testInput))}");
Console.WriteLine($"part1 result: {SolvePart1(ParseLines(input))}");

static int SolvePart1(Line[] input) 
{
    static IEnumerable<Point> PointsCovered(Line l)
    {
        if (l.IsVertical)
        {
            foreach(var y in Range(l.P1.Y, l.P2.Y))
            {
                yield return new Point(l.P1.X, y);
            }
        }
        if (l.IsHorizontal)
        {
            foreach(var x in Range(l.P1.X, l.P2.X))
            {
                yield return new Point(x, l.P1.Y);
            }
        }
    }
    return input.SelectMany(PointsCovered).GroupBy(x => x).Where(g => g.Count() >= 2).Count();
}

static IEnumerable<int> Range(int start, int stop)
{
    if (start < stop)
    {
        for (var i = start; i <= stop; i += 1)
        {
            yield return i;
        }
    }
    else if (start > stop)
    {
        for (var i = start; i >= stop; i -= 1)
        {
            yield return i;
        }
    }
}

static Point ParsePoint(string input)
{
    var s = input.Split(',');
    return new(int.Parse(s[0]), int.Parse(s[1]));
}

static Line ParseLine(string line)
{
    var points = line.Split("->");
    return new(ParsePoint(points[0]), ParsePoint(points[1]));
}

static Line[] ParseLines(string[] lines) =>
    lines.Select(ParseLine).ToArray();

record struct Point(int X, int Y);

record struct Line(Point P1, Point P2)
{
    public bool IsVertical => P1.X == P2.X;
    public bool IsHorizontal => P1.Y == P2.Y;
}

