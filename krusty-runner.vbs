Set ws = CreateObject("WScript.Shell" )
ws.Run "taskkill /t /f /im ""krustyboard.exe""", 0, True
ws.Run """C:\yakdat\code\git-repos\krustyboard\KrustyBoard\target\release\krustyboard.exe""", 0, False