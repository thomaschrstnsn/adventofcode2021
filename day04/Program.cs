var testInput =
@"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7".Split('\n');


var test = ParseGame(testInput);
Console.WriteLine($"part1 test: {SolvePart1(test)}");

var input = await System.IO.File.ReadAllLinesAsync("input.txt");
var game = ParseGame(input);
Console.WriteLine($"part1 result: {SolvePart1(game)}");


static BingoGame RunUntilDone(BingoGame game)
{
    var completedBoards = game.CompletedBoards;
    if (completedBoards.Any())
    {
        return game;
    }

    return RunUntilDone(game.MarkNext());
}

static int SolvePart1(BingoGame game)
{
    var complete = RunUntilDone(game);
    var winningBoard = complete.CompletedBoards.First();
    return winningBoard.UnmarkedNumbers.Sum() * complete.CurrentNumber;
}

static RowCells ParseRow(string line) => new RowCells(line
    .Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    .Select(int.Parse)
    .Select(n => new UnmarkedCell(n))
    .ToArray());

static (Board,IEnumerable<string> remaining) ParseBoard(IEnumerable<string> input)
{
    var boardLines = input.TakeWhile(l => !string.IsNullOrWhiteSpace(l));

    var rows = boardLines.Select(ParseRow).ToArray();
    var board = new Board(rows);

    var rest = input.SkipWhile(l => !string.IsNullOrWhiteSpace(l)).Skip(1);

    return (board, rest);
}

static BingoGame ParseGame(string[] lines)
{
    var numbers = lines.First().Split(',').Select(int.Parse).ToArray();

    var boardLines = lines.Skip(2);
    var numberOfBoards = lines.Count(l => string.IsNullOrWhiteSpace(l));
    var boards = new Board[numberOfBoards];
    var index = 0;
    while(boardLines.Any())
    {
        (var board, boardLines) = ParseBoard(boardLines);
        boards[index++] = board;
    }

    return new (numbers, boards, 0);
}

public interface IMarkable<T>
{
    T MarkNumber(int number);
}

public abstract record Cell(int Number) : IMarkable<Cell>
{
    public Cell MarkNumber(int n) => MarkIt(n);
    public abstract bool IsMarked {get;}
    protected abstract Cell MarkIt(int _);
};

public record MarkedCell(int number) : Cell(number) 
{
    protected override Cell MarkIt(int _) => this;
    public override bool IsMarked => true;
};

public record UnmarkedCell(int number) : Cell(number) 
{
    protected override Cell MarkIt(int n) => n == Number ? new MarkedCell(n) : this;
    public override bool IsMarked => false;
};

public record Board(RowCells[] Rows) : IMarkable<Board>
{
    public Board MarkNumber(int n) => new (Rows.Select(r => r.MarkNumber(n)).ToArray());

    public bool IsComplete => Rows.Any(r => r.IsComplete) || Enumerable.Range(0, Rows[0].Columns.Length).Any(ColumnComplete);

    public bool ColumnComplete(int c) => Rows.All(r => r.ColumnComplete(c));

    public IEnumerable<int> UnmarkedNumbers => Rows.SelectMany(r => r.UnmarkedNumbers);
};

public record RowCells(Cell[] Columns) : IMarkable<RowCells>
{
    public RowCells MarkNumber(int n) => new (Columns.Select(r => r.MarkNumber(n)).ToArray());

    public bool IsComplete => Columns.All(c => c.IsMarked);
    public bool ColumnComplete(int column) => Columns[column].IsMarked;

    public IEnumerable<int> UnmarkedNumbers => Columns.OfType<UnmarkedCell>().Select(c => c.Number);
};

public record BingoGame(int[] Numbers, Board[] Boards, int Index)
{
    public int NextNumber => Numbers[Index];
    public int CurrentNumber => Numbers[Index -1];

    public IEnumerable<Board> CompletedBoards => Boards.Where(b => b.IsComplete);

    public BingoGame MarkNext() => this with 
        {
            Boards = Boards.Select(b => b.MarkNumber(NextNumber)).ToArray(),
            Index = Index + 1
        };
};

