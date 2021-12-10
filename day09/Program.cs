var test =
@"2199943210
3987894921
9856789892
8767896789
9899965678".Split('\n').Select(s => s.Trim()).ToArray();

var inp = await System.IO.File.ReadAllLinesAsync("input.txt");

Console.WriteLine($"pt1 test: {SolvePt1(test)}");
Console.WriteLine($"pt1 result: {SolvePt1(inp)}");

static int SolvePt1(string[] inp)
{
    var sum = 0;
    foreach(var x in Enumerable.Range(0, inp.Length))
    {
        foreach(var y in Enumerable.Range(0, inp[x].Length))
        {
            var val = inp.GetValueOrDefault(x,y);
            var others = new []{(x-1, y), (x+1, y), (x, y-1), (x,y+1)}.Select(_ => inp.GetValueOrDefault(_.Item1, _.Item2));
            if (others.All(o => o > val))
            {
                sum += (val+1);
            }
        }
    }
    return sum;
};

public static class Ex 
{
    public static int GetValueOrDefault(this string[] arr, int i1, int i2)
    {
        if (i1 < 0 || i1 >= arr.Length) return int.MaxValue;
        var chars = arr[i1].ToCharArray();
        if (i2 < 0 || i2 >= chars.Length) return int.MaxValue;
        var c = chars[i2];

        return int.Parse(new string(new[]{c}));
    }
}

// static string[] Parse(string[] lines) =>
//     lines.Select(l => l.Split("").Select(int.Parse).ToArray()).ToArray();

