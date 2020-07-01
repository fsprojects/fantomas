using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

namespace Fantomas.EditorConfig.Core
{
	/// <summary>
	/// Represents an ini section within the editorconfig file
	/// </summary>
	internal class IniSection : Dictionary<string, string>
	{
		public string Name { get; set; }
	}

	/// <summary>
	/// Represents the raw config file as INI
	/// </summary>
	internal class EditorConfigFile
	{
		private readonly Regex _section = new Regex(@"^\s*\[(([^#;]|\\#|\\;)+)\]\s*([#;].*)?$");
		private readonly Regex _comment = new Regex(@"^\s*[#;]");
		private readonly Regex _property = new Regex(@"^\s*([\w\.\-_]+)\s*[=:]\s*(.*?)\s*([#;].*)?$");
		
		public IniSection Global = new IniSection(); 
		public List<IniSection> Sections = new List<IniSection>(); 

		public string Directory { get; private set; }
		private bool _isRoot = false;
		public bool IsRoot { get { return _isRoot; }}

		public EditorConfigFile(string file)
		{
			this.Directory = Path.GetDirectoryName(file);
			this.Parse(file);

			if (this.Global.ContainsKey("root"))
				bool.TryParse(this.Global["root"], out this._isRoot);

		}

		public void Parse(string file)
		{
			var lines = File.ReadLines(file);

			var activeSection = this.Global;
			foreach (var line in lines)
			{
				if (_comment.IsMatch(line)) continue;
				var matches = _property.Matches(line);
				if (matches.Count > 0)
				{
					var key = matches[0].Groups[1].Value.Trim();
					var value = matches[0].Groups[2].Value.Trim();
					activeSection.Add(key, value);
					continue;
				}
				matches = _section.Matches(line);
				if (matches.Count <= 0) continue;

				var sectionName = matches[0].Groups[1].Value;
				activeSection = new IniSection { Name = sectionName };
				this.Sections.Add(activeSection);
			}
		}
	}
}
