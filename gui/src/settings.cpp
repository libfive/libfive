#include <QDebug>
#include <QGridLayout>
#include <QDoubleSpinBox>
#include <QLabel>

#include "gui/settings.hpp"

Settings::Settings(QObject* parent)
    : QObject(parent)
{
    // Nothing to do here
}

void Settings::openPane()
{
    if (pane.data() == nullptr)
    {
        pane = new SettingsPane(min, max, res);
        connect(pane, &SettingsPane::changed,
                this, &Settings::applyChange);
        pane->show();
    }
}

void Settings::applyChange(QVector3D _min, QVector3D _max, float _res)
{
    min = _min;
    max = _max;
    res = _res;

    qDebug() << "Got change" << min << max << res;
    emit(changed(min, max, res));
}

////////////////////////////////////////////////////////////////////////////////

SettingsPane::SettingsPane(QVector3D min, QVector3D max, float _res)
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
    layout->addWidget(new QLabel("Resolution:"), 4, 0);
    layout->addWidget(res, 4, 1);

    layout->setMargin(4);
    layout->setSpacing(2);

    for (auto t : {xmin, xmax, ymin, ymax, zmin, zmax})
    {
        t->setMinimum(-100);
        t->setMaximum(100);
    }
    res->setMaximum(100);

    xmin->setValue(min.x());
    ymin->setValue(min.y());
    zmin->setValue(min.z());
    xmax->setValue(max.x());
    ymax->setValue(max.y());
    zmax->setValue(max.z());
    res->setValue(_res);

    for (auto t : {xmin, xmax, ymin, ymax, zmin, zmax, res})
    {
        connect(t, static_cast<void (QDoubleSpinBox::*)(double)>
                    (&QDoubleSpinBox::valueChanged),
                this, [=](double){
                    emit(this->changed(
                        QVector3D(xmin->value(), ymin->value(), zmin->value()),
                        QVector3D(xmax->value(), ymax->value(), zmax->value()),
                        res->value())); } );
    }

    setLayout(layout);
    setWindowFlags(Qt::Tool | Qt::CustomizeWindowHint |
                   Qt::WindowTitleHint | Qt::WindowCloseButtonHint);
}
