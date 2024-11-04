using System.Printing;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Simple_Interpreter_Fs;




namespace WpfApp1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    /// 
    
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            
            InitializeComponent();

        }
        private void OnFocus(object sender, RoutedEventArgs e)
        {
            if (TBOX.Text == "Type here")
            {
                
                TBOX.Clear();
            }


        }
        private void OffFocus(object sender, RoutedEventArgs e)
        {
            if (TBOX.Text == "")
            {
                TBOX.Text = "Type here";
            }

        }
        private void OnClick(object sender, RoutedEventArgs e)
        {
            Outbox.Text = Say.Interpreter(TBOX.Text).Item2.ToString();
        }

        private void TBOX_TextChanged(object sender, TextChangedEventArgs e)
        {

            if (TBOX.Text.Length >= 32)
            {
                LenWarn.Visibility = Visibility.Hidden;
            }
            else if (TBOX.Text.Length < 32)
            {
                LenWarn.Visibility = Visibility.Visible;
            }

        }
    }
}