namespace Fantomas.EditorConfig.Core
{
	/// <summary>
	/// a whole number defining the number of columns used for each indentation level and the width of soft tabs (when supported). 
	/// When set to tab, the value of tab_width (if specified) will be used.
	/// </summary>
	public class IndentSize
	{
		public bool Unset { get; private set; }
		public bool UseTabWidth { get; private set; }
		public int? NumberOfColumns { get; private set; }
		
		public IndentSize()
		{
			this.Unset = true;
		}

		public IndentSize(bool useTabs)
		{
			this.UseTabWidth = useTabs;
		}

		public IndentSize(int numberOfColumns)
		{
			this.NumberOfColumns = numberOfColumns;
		}
	}
}