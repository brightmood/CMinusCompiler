extern int puts(String s);

int max(int a, int b)
{
    String s = "abc";
    if(a>b)
    {
        puts(s);
        return a;
    }

    else
        return b;
}

int main()
{
    int a = 2;
    int b = 1;
    int c = max(a, b);
}