#include <cassert>
#include <cmath>

#include <QGridLayout>
#include <QLabel>

#include "gui/settings.hpp"

QRegularExpression Settings::settings_regex(
        "#! RENDER (.*) (.*) (.*) / (.*) (.*) (.*) / (.*) !#");
QString Settings::settings_fmt(
        "#! RENDER %1 %2 %3 / %4 %5 %6 / %7 !#");

Settings::Settings()
    : res(-1)
{
    // Nothing to do here
}

Settings::Settings(QVector3D min, QVector3D max, float res)
    : min(min), max(max), res(res)
{
    const float volume = (max.x() - min.x()) *
                         (max.y() - min.y()) *
                         (max.z() - min.z());

    // Magic numbers from Antimony's mesh export dialog
    const float target_res = pow((1 << 22) / volume, 0.33) / 2.52;

    // More magic numbers
    div = (target_res > res) ? 0
        : int(log(res / target_res) / log(2) + 0.5);
}

Settings::Settings(QVector3D min, QVector3D max, float res, int div)
    : min(min), max(max), res(res), div(div)
{
    // Nothing to do here
}

Settings Settings::next() const
{
    return div > 0 ? Settings(min, max, res, div - 1) : Settings();
}

SettingsPane::SettingsPane(Settings s)
    : xmin(new QDoubleSpinBox),
      xmax(new QDoubleSpinBox),
      ymin(new QDoubleSpinBox),
      ymax(new QDoubleSpinBox),
      zmin(new QDoubleSpinBox),
      zmax(new QDoubleSpinBox),
      res(new QDoubleSpinBox)
{
    auto layout = new QGridLayout();

    layout->addWidget(new QLabel("<b>Bounds:</b>"), 0, 1);
    layout->addWidget(new QLabel("X:"), 1, 0);
    layout->addWidget(xmin, 1, 1);
    layout->addWidget(xmax, 1, 2);
    layout->addWidget(new QLabel("Y:"), 2, 0);
    layout->addWidget(ymin, 2, 1);
    layout->addWidget(ymax, 2, 2);
    layout->addWidget(new QLabel("Z:"), 3, 0);
    layout->addWidget(zmin, 3, 1);
    layout->addWidget(zmax, 3, 2);

    QFrame* line = new QFrame();
    line->setFrameShape(QFrame::HLine);
    line->setFrameShadow(QFrame::Raised);
    layout->addWidget(line, 4, 0, 1, 3);

    layout->addWidget(new QLabel("Resolution:"), 5, 0, 1, 2, Qt::AlignCenter);
    layout->addWidget(res, 5, 2);

    layout->setMargin(10);
    layout->setSpacing(4);

    for (auto t : {xmin, xmax, ymin, ymax, zmin, zmax})
    {
        t->setMinimum(-100);
        t->setMaximum(100);
    }
    res->setMaximum(100);

    set(s);

    for (auto t : {xmin, xmax, ymin, ymax, zmin, zmax, res})
    {
        connect(t, static_cast<void (QDoubleSpinBox::*)(double)>
                    (&QDoubleSpinBox::valueChanged),
                this, [=](double){
                    emit(this->changed(Settings(
                        QVector3D(xmin->value(), ymin->value(), zmin->value()),
                        QVector3D(xmax->value(), ymax->value(), zmax->value()),
                        res->value()))); } );
    }

    setLayout(layout);
    setWindowFlags(Qt::Tool | Qt::CustomizeWindowHint |
                   Qt::WindowTitleHint | Qt::WindowCloseButtonHint);
    setAttribute(Qt::WA_DeleteOnClose);
}

void SettingsPane::set(Settings s)
{
    xmin->setValue(s.min.x());
    ymin->setValue(s.min.y());
    zmin->setValue(s.min.z());
    xmax->setValue(s.max.x());
    ymax->setValue(s.max.y());
    zmax->setValue(s.max.z());
    res->setValue(s.res);
}
