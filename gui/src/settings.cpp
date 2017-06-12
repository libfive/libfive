#include <QDebug>
#include <QGridLayout>
#include <QDoubleSpinBox>
#include <QLabel>

#include "gui/settings.hpp"

SettingsPane::SettingsPane(Settings s)
{
    auto layout = new QGridLayout();
    auto xmin = new QDoubleSpinBox;
    auto xmax = new QDoubleSpinBox;
    auto ymin = new QDoubleSpinBox;
    auto ymax = new QDoubleSpinBox;
    auto zmin = new QDoubleSpinBox;
    auto zmax = new QDoubleSpinBox;
    auto res = new QDoubleSpinBox;

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

    xmin->setValue(s.min.x());
    ymin->setValue(s.min.y());
    zmin->setValue(s.min.z());
    xmax->setValue(s.max.x());
    ymax->setValue(s.max.y());
    zmax->setValue(s.max.z());
    res->setValue(s.res);

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
