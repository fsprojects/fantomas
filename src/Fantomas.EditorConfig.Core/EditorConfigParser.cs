using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace Fantomas.EditorConfig.Core
{
	/// <summary>
	/// The EditorConfigParser locates all relevant editorconfig files and makes sure they are merged correctly.
	/// </summary>
	public class EditorConfigParser
	{
		/// <summary>
		/// The current (and latest parser supported) version as string
		/// </summary>
		public static readonly string VersionString = "0.12.1";

		/// <summary>
		/// The current editorconfig version
		/// </summary>
		public static readonly Version Version = new Version(VersionString);

		private readonly GlobMatcherOptions _globOptions = new GlobMatcherOptions { MatchBase = true, Dot = true, AllowWindowsPaths = true };
		
		/// <summary>
		/// The configured name of the files holding editorconfig values, defaults to ".editorconfig"
		/// </summary>
		public string ConfigFileName { get; private set; }
		
		/// <summary>
		/// The editor config parser version in use, defaults to latest <see cref="EditorConfigParser.Version"/>
		/// </summary>
		public Version ParseVersion { get; private set; }
		
		/// <summary>
		/// The EditorConfigParser locates all relevant editorconfig files and makes sure they are merged correctly.
		/// </summary>
		/// <param name="configFileName">The name of the file(s) holding the editorconfiguration values</param>
		/// <param name="developmentVersion">Only used in testing, development to pass an older version to the parsing routine</param>
		public EditorConfigParser(string configFileName = ".editorconfig", Version developmentVersion = null)
		{
			ConfigFileName = configFileName ?? ".editorconfig";
			ParseVersion = developmentVersion ?? Version;
		}

		/// <summary>
		/// Gets the FileConfiguration for each of the passed fileName by resolving their relevant editorconfig files.
		/// </summary>
		public IEnumerable<FileConfiguration> Parse(params string[] fileNames)
		{
			return fileNames
				.Select(this.Parse)
				.ToList();
		}

		public FileConfiguration Parse(string fileName)
		{
			var file = fileName.Trim().Trim(new[] {'\r', '\n'});
			Debug.WriteLine(":: {0} :: {1}", this.ConfigFileName, file);

			var fullPath = Path.GetFullPath(file).Replace(@"\", "/");
			var configFiles = this.AllParentConfigFiles(fullPath);

			//All the .editorconfig files going from root =>.fileName
			var editorConfigFiles = this.ParseConfigFilesTillRoot(configFiles).Reverse();

			var sections =
				from configFile in editorConfigFiles
				from section in configFile.Sections
				let glob = this.FixGlob(section.Name, configFile.Directory)
				where this.IsMatch(glob, fullPath, configFile.Directory)
				select section;

			var allProperties =
				from section in sections
				from kv in section
				select FileConfiguration.Sanitize(kv.Key, kv.Value);

			var properties = new Dictionary<string, string>();
			foreach (var kv in allProperties)
				properties[kv.Key] = kv.Value;

			return new FileConfiguration(ParseVersion, file, properties);
		}

		private bool IsMatch(string glob, string fileName, string directory)
		{
			var matcher = GlobMatcher.Create(glob, _globOptions);
			var isMatch = matcher.IsMatch(fileName);
			Debug.WriteLine("{0} :: {1} \t\t:: {2}", isMatch ? "?" : "?", glob, fileName);
			return isMatch;
		}

		private string FixGlob(string glob, string directory)
		{
			switch (glob.IndexOf('/'))
			{
				case -1: glob = "**/" + glob; break;
				case 0: glob = glob.Substring(1); break;
			}
			
			//glob = Regex.Replace(glob, @"\*\*", "{*,**/**/**}");

			directory = directory.Replace(@"\", "/");
			if (!directory.EndsWith("/")) directory += "/";

			return directory + glob;
		}

		private IEnumerable<EditorConfigFile> ParseConfigFilesTillRoot(IEnumerable<string> configFiles)
		{
			foreach (var configFile in configFiles.Select(f=> new EditorConfigFile(f)))
			{
				yield return configFile;
				if (configFile.IsRoot) yield break;
			}
		}

		private IEnumerable<string> AllParentConfigFiles(string fullPath)
		{
			return from parent in this.AllParentDirectories(fullPath)
				   let configFile = Path.Combine(parent, this.ConfigFileName)
				   where File.Exists(configFile)
				   select configFile;
		}

		private IEnumerable<string> AllParentDirectories(string fullPath)
		{
			var root = new DirectoryInfo(fullPath).Root.FullName;
			var dir = Path.GetDirectoryName(fullPath);
			do
			{
				if (dir == null) yield break;
				yield return dir;
				var dirInfo = new DirectoryInfo(dir);
				dir = dirInfo.Parent.FullName;
			} while (dir != root);
		}
	}
}