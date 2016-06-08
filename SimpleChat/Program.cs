using System;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Threading;

class Program
{
    static StreamReader reader;

    public static void GetMessages()
    {
        string message = string.Empty;
        while (true)
        {
            message = reader.ReadLine();
            if (message != string.Empty)
            {
                Console.ForegroundColor = ConsoleColor.Blue;
                Console.WriteLine("\b\b> " + message);
                Console.ForegroundColor = ConsoleColor.Gray;
                Console.Write("> ");
            }
        }
    }


    static void Main(string[] args)
    {
        bool server = false;
        Console.Write("Вы сервер(1) или клиент(2): ");
        server = int.Parse(Console.ReadLine()) == 1 ? true : false;
        if (server)
        {
            Console.WriteLine("Жду клиента..");
            TcpListener listener = new TcpListener(5000);
            listener.Start();
            TcpClient client = listener.AcceptTcpClient();
            StreamWriter writer = new StreamWriter(client.GetStream());
            writer.AutoFlush = true;
            reader = new StreamReader(client.GetStream());
            Thread thread = new Thread(new ThreadStart(delegate() { GetMessages(); }));
            thread.Start();
            string msg = string.Empty;
            while (true)
            {
                Console.Write("> ");
                msg = Console.ReadLine();
                writer.WriteLine(msg);
            }
        }
        else
        {
            Console.Write("Введите IP - адрес: ");
            string ipAddress = Console.ReadLine();
            TcpClient client = new TcpClient();
            client.Connect(IPAddress.Parse(ipAddress), 5000);
            
            StreamWriter sWriter = new StreamWriter(client.GetStream());
            sWriter.AutoFlush = true;
            reader = new StreamReader(client.GetStream());
            Thread thread = new Thread(new ThreadStart(delegate() { GetMessages(); }));
            thread.Start();
            string msg = string.Empty;
            while (true)
            {
                Console.Write("> ");
                msg = Console.ReadLine();
                sWriter.WriteLine(msg);
            }
        }
    }
}