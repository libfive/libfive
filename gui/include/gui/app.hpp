#include <QApplication>

class App : public QApplication
{
    Q_OBJECT
public:
    explicit App(int& argc, char **argv);
};
