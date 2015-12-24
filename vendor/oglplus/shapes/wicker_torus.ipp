/**
 *  @file oglplus/shapes/wicker_torus.ipp
 *  @brief Implementation of shapes::WickerTorus
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace shapes {

OGLPLUS_LIB_FUNC
std::vector<GLfloat> WickerTorus::_positions(void) const
{
	typedef GLfloat T;
	std::vector<GLfloat> dest(
		2*2*2*_rings*(_sections*3 + 1)*3+
		2*2*2*_sections*(_rings*2 + 1)*3
	);
	unsigned k = 0;
	//
	const GLdouble t = _thickness / _radius_in;
	const GLdouble r_step = (math::TwoPi()) / GLdouble(_rings);
	const GLdouble s_step = (math::TwoPi()) / GLdouble(_sections);
	const GLdouble r_slip = r_step * _r_slip_coef;
	const GLdouble s_slip = s_step * _s_slip_coef;
	const GLdouble r1 = _radius_in;
	const GLdouble r2 = _radius_out - _radius_in;

	for(unsigned f=0; f!=2; ++f)
	{
		const GLdouble f_sign = (f == 0)? 1.0: -1.0;
		const GLdouble fdt = t*f_sign*0.5;
		const GLdouble rfs = f_sign * r_slip;
		for(unsigned r=0; r!=_rings; ++r)
		{
			const GLdouble r_angle = r*r_step;
			const GLdouble r_sign = (r % 2 == 0)? 1.0: -1.0;
			const double rdt = t*r_sign*2.0;
			for(unsigned s=0; s!=_sections; ++s)
			{
				const GLdouble sa[3] = {
					s*s_step,
					(s + t)*s_step,
					(s + 1.0 - 2.0*t)*s_step
				};
				const GLdouble rd[3] = {
					fdt,
					fdt+((s % 2 == 0)? -rdt : rdt),
					fdt+((s % 2 == 0)? -rdt : rdt)
				};
				for(unsigned p=0; p!=3; ++p)
				{
					GLdouble vr = std::cos(sa[p]);
					GLdouble vy = std::sin(sa[p]);
					GLdouble vs = 0.5 + vr*0.5;
					for(unsigned d=0; d!=2; ++d)
					{
						const GLdouble d_sign = (d == 0)?-1.0: 1.0;

						GLdouble rs_angle = r_angle + d_sign*rfs*(1.0 - 0.25*vs);

						GLdouble vx = std::cos(rs_angle);
						GLdouble vz = std::sin(rs_angle);

						dest[k++] = T(vx*(r1 + r2*(1.0 + vr) + rd[p]*vr));
						dest[k++] = T(vy*(r2 + rd[p]));
						dest[k++] = T(vz*(r1 + r2*(1.0 + vr) + rd[p]*vr));
					}
				}
			}

			for(unsigned d=0; d!=2; ++d)
			{
				const GLdouble d_sign = (d == 0)?-1.0: 1.0;
				GLdouble rs_angle = r_angle + d_sign*rfs*0.75;

				GLdouble vx = std::cos(rs_angle);
				GLdouble vz = std::sin(rs_angle);
				dest[k++] = T(vx*(r1 + r2*(2.0) + fdt));
				dest[k++] = T(0.0);
				dest[k++] = T(vz*(r1 + r2*(2.0) + fdt));
			}
		}
	}

	for(unsigned d=0; d!=2; ++d)
	{
		const GLdouble d_sign = (d == 0)? 1.0: -1.0;
		const GLdouble rds = d_sign * r_slip;
		for(unsigned r=0; r!=_rings; ++r)
		{
			const GLdouble r_angle = r*r_step;
			const GLdouble r_sign = (r % 2 == 0)? 1.0: -1.0;
			const double rdt = t*r_sign*2.0;
			for(unsigned s=0; s!=_sections; ++s)
			{
				const GLdouble sa[3] = {
					s*s_step,
					(s + t)*s_step,
					(s + 1.0 - 2*t)*s_step
				};
				const GLdouble rd[3] = {
					0.0,
					0.0+((s % 2 == 0)? -rdt : rdt),
					0.0+((s % 2 == 0)? -rdt : rdt)
				};
				for(unsigned p=0; p!=3; ++p)
				{
					GLdouble vr = std::cos(sa[p]);
					GLdouble vy = std::sin(sa[p]);
					GLdouble vs = 0.5 + vr*0.5;

					GLdouble rs_angle = r_angle + rds*(1.0 - 0.25*vs);

					GLdouble vx = std::cos(rs_angle);
					GLdouble vz = std::sin(rs_angle);

					for(unsigned f=0; f!=2; ++f)
					{
						const GLdouble f_sign = (f == 0)? 1.0: -1.0;
						const GLdouble fdt = 0.5*t*f_sign*d_sign;
						dest[k++] = T(vx*(r1 + r2*(1.0 + vr) + (fdt+rd[p])*vr));
						dest[k++] = T(vy*(r2 + (fdt+rd[p])));
						dest[k++] = T(vz*(r1 + r2*(1.0 + vr) + (fdt+rd[p])*vr));
					}
				}
			}
			GLdouble rs_angle = r_angle + rds*0.75;

			GLdouble vx = std::cos(rs_angle);
			GLdouble vz = std::sin(rs_angle);
			for(unsigned f=0; f!=2; ++f)
			{
				const GLdouble f_sign = (f == 0)? 1.0: -1.0;
				const GLdouble fdt = 0.5*t*f_sign*d_sign;
				dest[k++] = T(vx*(r1 + r2*(2.0) + fdt));
				dest[k++] = T(0.0);
				dest[k++] = T(vz*(r1 + r2*(2.0) + fdt));
			}
		}
	}

	for(unsigned f=0; f!=2; ++f)
	{
		const GLdouble f_sign = (f == 0)? 1.0: -1.0;
		const GLdouble fdt = t*f_sign*0.95;
		for(unsigned s=0; s!=_sections; ++s)
		{
			const GLdouble s_angle = s_step*0.5 + s*s_step;
			const GLdouble sa[2] = {
				s_angle + s_slip*f_sign,
				s_angle - s_slip*f_sign
			};
			for(unsigned r=0; r!=_rings; ++r)
			{
				const GLdouble r_angle = r*r_step;
				const GLdouble ra[2] = {
					r_angle + r_slip,
					r_angle + r_step - r_slip
				};
				for(unsigned p=0; p!=2; ++p)
				{
					GLdouble vx = std::cos(ra[p]);
					GLdouble vz = std::sin(ra[p]);

					for(unsigned d=0; d!=2; ++d)
					{
						GLdouble vr = std::cos(sa[d]);
						GLdouble vy = std::sin(sa[d]);

						dest[k++] = T(vx*(r1 + r2*(1.0 + vr) + fdt*vr));
						dest[k++] = T(vy*(r2 + fdt));
						dest[k++] = T(vz*(r1 + r2*(1.0 + vr) + fdt*vr));
					}
				}
			}
			GLdouble vx = std::cos(r_slip);
			GLdouble vz = std::sin(r_slip);

			for(unsigned d=0; d!=2; ++d)
			{
				GLdouble vr = std::cos(sa[d]);
				GLdouble vy = std::sin(sa[d]);

				dest[k++] = T(vx*(r1 + r2*(1.0 + vr) + fdt*vr));
				dest[k++] = T(vy*(r2 + fdt));
				dest[k++] = T(vz*(r1 + r2*(1.0 + vr) + fdt*vr));
			}
		}
	}

	for(unsigned d=0; d!=2; ++d)
	{
		const GLdouble d_sign = (d == 0)? 1.0: -1.0;
		for(unsigned s=0; s!=_sections; ++s)
		{
			const GLdouble s_angle = s_step*0.5 + s*s_step;
			const GLdouble sa = s_angle + s_slip*d_sign;
			const GLdouble vr = std::cos(sa);
			const GLdouble vy = std::sin(sa);
			for(unsigned r=0; r!=_rings; ++r)
			{
				const GLdouble r_angle = r*r_step;
				const GLdouble ra[2] = {
					r_angle + r_slip,
					r_angle + r_step - r_slip
				};
				for(unsigned p=0; p!=2; ++p)
				{
					GLdouble vx = std::cos(ra[p]);
					GLdouble vz = std::sin(ra[p]);

					for(unsigned f=0; f!=2; ++f)
					{
						const GLdouble f_sign = (f == 0)? 1.0: -1.0;
						const GLdouble fdt = -t*d_sign*f_sign*0.95;

						dest[k++] = T(vx*(r1 + r2*(1.0 + vr) + fdt*vr));
						dest[k++] = T(vy*(r2 + fdt));
						dest[k++] = T(vz*(r1 + r2*(1.0 + vr) + fdt*vr));
					}
				}
			}
			GLdouble vx = std::cos(r_slip);
			GLdouble vz = std::sin(r_slip);

			for(unsigned f=0; f!=2; ++f)
			{
				const GLdouble f_sign = (f == 0)? 1.0: -1.0;
				const GLdouble fdt = -t*d_sign*f_sign*0.5;

				dest[k++] = T(vx*(r1 + r2*(1.0 + vr) + fdt*vr));
				dest[k++] = T(vy*(r2 + fdt));
				dest[k++] = T(vz*(r1 + r2*(1.0 + vr) + fdt*vr));
			}
		}
	}

	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> WickerTorus::_normals(void) const
{
	typedef GLfloat T;
	std::vector<GLfloat> dest(
		2*2*2*_rings*(_sections*3 + 1)*3+
		2*2*2*_sections*(_rings*2 + 1)*3
	);
	unsigned k = 0;
	//
	const GLdouble r_step = (math::TwoPi()) / GLdouble(_rings);
	const GLdouble s_step = (math::TwoPi()) / GLdouble(_sections);
	const GLdouble r_slip = r_step * _r_slip_coef;
	const GLdouble s_slip = s_step * _s_slip_coef;
	const GLdouble s_slop = (math::Pi()) / 4.0;

	for(unsigned f=0; f!=2; ++f)
	{
		const GLdouble f_sign = (f == 0)? 1.0: -1.0;
		for(unsigned r=0; r!=_rings; ++r)
		{
			GLdouble vx = std::cos(r*r_step);
			GLdouble vz = std::sin(r*r_step);
			const GLdouble rslp = s_slop*f_sign;
			for(unsigned s=0; s!=_sections; ++s)
			{
				const GLdouble sa[3] = {
					s*s_step+((s % 2 == 0)?-rslp:rslp),
					s*s_step,
					s*s_step
				};
				for(unsigned p=0; p!=3; ++p)
				{
					GLdouble vr = std::cos(sa[p]);
					GLdouble vy = std::sin(sa[p]);
					for(unsigned d=0; d!=2; ++d)
					{
						dest[k++] = T(f_sign*vx*vr);
						dest[k++] = T(f_sign*vy);
						dest[k++] = T(f_sign*vz*vr);
					}
				}
			}
			for(unsigned d=0; d!=2; ++d)
			{
				dest[k++] = T(f_sign*vx);
				dest[k++] = T(f_sign*std::sin(-rslp));
				dest[k++] = T(f_sign*vz);
			}
		}
	}

	for(unsigned d=0; d!=2; ++d)
	{
		const GLdouble d_sign = (d == 0)?-1.0: 1.0;
		for(unsigned r=0; r!=_rings; ++r)
		{
			GLdouble vx = std::cos(r*r_step);
			GLdouble vz = std::sin(r*r_step);
			for(unsigned s=0; s!=_sections; ++s)
			{
				for(unsigned p=0; p!=3; ++p)
				{
					for(unsigned f=0; f!=2; ++f)
					{
						dest[k++] = T(+vz*d_sign);
						dest[k++] = T(0);
						dest[k++] = T(-vx*d_sign);
					}
				}
			}
			for(unsigned f=0; f!=2; ++f)
			{
				dest[k++] = T(+vz*d_sign);
				dest[k++] = T(0);
				dest[k++] = T(-vx*d_sign);
			}
		}
	}

	for(unsigned f=0; f!=2; ++f)
	{
		const GLdouble f_sign = (f == 0)? 1.0: -1.0;
		for(unsigned s=0; s!=_sections; ++s)
		{
			const GLdouble s_angle = s_step*0.5 + s*s_step;
			const GLdouble sa[2] = {
				s_angle + s_slip*f_sign,
				s_angle - s_slip*f_sign
			};
			for(unsigned r=0; r!=_rings; ++r)
			{
				const GLdouble r_angle = r*r_step;
				const GLdouble ra[2] = {
					r_angle + r_slip,
					r_angle + r_step - r_slip
				};
				for(unsigned p=0; p!=2; ++p)
				{
					GLdouble vx = std::cos(ra[p]);
					GLdouble vz = std::sin(ra[p]);

					for(unsigned d=0; d!=2; ++d)
					{
						GLdouble vr = std::cos(sa[d]);
						GLdouble vy = std::sin(sa[d]);

						dest[k++] = T(f_sign*vx*vr);
						dest[k++] = T(f_sign*vy);
						dest[k++] = T(f_sign*vz*vr);
					}
				}
			}
			GLdouble vx = std::cos(r_slip);
			GLdouble vz = std::sin(r_slip);

			for(unsigned d=0; d!=2; ++d)
			{
				GLdouble vr = std::cos(sa[d]);
				GLdouble vy = std::sin(sa[d]);

				dest[k++] = T(f_sign*vx*vr);
				dest[k++] = T(f_sign*vy);
				dest[k++] = T(f_sign*vz*vr);
			}
		}
	}

	for(unsigned d=0; d!=2; ++d)
	{
		const GLdouble d_sign = (d == 0)? 1.0: -1.0;
		for(unsigned s=0; s!=_sections; ++s)
		{
			const GLdouble s_angle = s_step*0.5 + s*s_step;
			const GLdouble sa = s_angle + s_slip*d_sign;
			const GLdouble vr = std::sin(sa);
			const GLdouble vy = std::cos(sa);
			for(unsigned r=0; r!=_rings; ++r)
			{
				const GLdouble r_angle = r*r_step;
				const GLdouble ra[2] = {
					r_angle + r_slip,
					r_angle + r_step - r_slip
				};
				for(unsigned p=0; p!=2; ++p)
				{
					GLdouble vx = std::cos(ra[p]);
					GLdouble vz = std::sin(ra[p]);

					for(unsigned f=0; f!=2; ++f)
					{
						dest[k++] = T(d_sign*-vx*vr);
						dest[k++] = T(d_sign*vy);
						dest[k++] = T(d_sign*-vz*vr);
					}
				}
			}
			GLdouble vx = std::cos(r_slip);
			GLdouble vz = std::sin(r_slip);

			for(unsigned f=0; f!=2; ++f)
			{
				dest[k++] = T(d_sign*-vx*vr);
				dest[k++] = T(d_sign*vy);
				dest[k++] = T(d_sign*-vz*vr);
			}
		}
	}

	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> WickerTorus::_tangents(void) const
{
	typedef GLfloat T;
	std::vector<GLfloat> dest(
		2*2*2*_rings*(_sections*3 + 1)*3+
		2*2*2*_sections*(_rings*2 + 1)*3
	);
	unsigned k = 0;
	//
	const GLdouble r_step = (math::TwoPi()) / GLdouble(_rings);
	const GLdouble s_step = (math::TwoPi()) / GLdouble(_sections);
	const GLdouble r_slip = r_step * _r_slip_coef;
	const GLdouble s_slop = (math::Pi()) / 4.0;

	for(unsigned f=0; f!=2; ++f)
	{
		const GLdouble f_sign = (f == 0)? 1.0: -1.0;
		for(unsigned r=0; r!=_rings; ++r)
		{
			GLdouble vx = std::cos(r*r_step);
			GLdouble vz = std::sin(r*r_step);
			for(unsigned s=0; s!=_sections; ++s)
			{
				for(unsigned p=0; p!=3; ++p)
				{
					for(unsigned d=0; d!=2; ++d)
					{
						dest[k++] = T(+vz*f_sign);
						dest[k++] = T(0);
						dest[k++] = T(-vx*f_sign);
					}
				}
			}
			for(unsigned d=0; d!=2; ++d)
			{
				dest[k++] = T(+vz*f_sign);
				dest[k++] = T(0);
				dest[k++] = T(-vx*f_sign);
			}
		}
	}

	for(unsigned d=0; d!=2; ++d)
	{
		const GLdouble d_sign = (d == 0)? 1.0: -1.0;
		for(unsigned r=0; r!=_rings; ++r)
		{
			GLdouble r_sign = r % 2 == 0 ? 1.0: -1.0;
			GLdouble vx = std::cos(r*r_step);
			GLdouble vz = std::sin(r*r_step);
			const GLdouble rslp = s_slop*r_sign;
			for(unsigned s=0; s!=_sections; ++s)
			{
				const GLdouble sa[3] = {
					s*s_step+((s % 2 == 0)?rslp:-rslp),
					s*s_step,
					s*s_step
				};
				for(unsigned p=0; p!=3; ++p)
				{
					GLdouble vr = std::cos(sa[p]);
					GLdouble vy = std::sin(sa[p]);
					for(unsigned f=0; f!=2; ++f)
					{
						dest[k++] = T(d_sign*vx*vr);
						dest[k++] = T(d_sign*vy);
						dest[k++] = T(d_sign*vz*vr);
					}
				}
			}
			for(unsigned f=0; f!=2; ++f)
			{
				dest[k++] = T(d_sign*vx);
				dest[k++] = T(d_sign*std::sin(rslp));
				dest[k++] = T(d_sign*vz);
			}
		}
	}

	for(unsigned f=0; f!=2; ++f)
	{
		const GLdouble f_sign = (f == 0)? 1.0: -1.0;
		for(unsigned s=0; s!=_sections; ++s)
		{
			for(unsigned r=0; r!=_rings; ++r)
			{
				const GLdouble r_angle = r*r_step;
				const GLdouble ra[2] = {
					r_angle + r_slip,
					r_angle + r_step - r_slip
				};
				for(unsigned p=0; p!=2; ++p)
				{
					GLdouble vx = std::cos(ra[p]);
					GLdouble vz = std::sin(ra[p]);

					for(unsigned d=0; d!=2; ++d)
					{
						dest[k++] = T(+vz*f_sign);
						dest[k++] = T(0);
						dest[k++] = T(-vx*f_sign);
					}
				}
			}
			GLdouble vx = std::cos(r_slip);
			GLdouble vz = std::sin(r_slip);

			for(unsigned d=0; d!=2; ++d)
			{
				dest[k++] = T(+vz*f_sign);
				dest[k++] = T(0);
				dest[k++] = T(-vx*f_sign);
			}
		}
	}

	for(unsigned d=0; d!=2; ++d)
	{
		const GLdouble d_sign = (d == 0)? 1.0: -1.0;
		for(unsigned s=0; s!=_sections; ++s)
		{
			for(unsigned r=0; r!=_rings; ++r)
			{
				const GLdouble r_angle = r*r_step;
				const GLdouble ra[2] = {
					r_angle + r_slip,
					r_angle + r_step - r_slip
				};
				for(unsigned p=0; p!=2; ++p)
				{
					GLdouble vx = std::cos(ra[p]);
					GLdouble vz = std::sin(ra[p]);

					for(unsigned f=0; f!=2; ++f)
					{
						dest[k++] = T(+vz*d_sign);
						dest[k++] = T(0);
						dest[k++] = T(-vx*d_sign);
					}
				}
			}
			GLdouble vx = std::cos(r_slip);
			GLdouble vz = std::sin(r_slip);

			for(unsigned f=0; f!=2; ++f)
			{
				dest[k++] = T(+vz*d_sign);
				dest[k++] = T(0);
				dest[k++] = T(-vx*d_sign);
			}
		}
	}

	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> WickerTorus::_bitangents(void) const
{
	typedef GLfloat T;
	std::vector<T> nmls, tgts;
	Normals(nmls);
	Tangents(tgts);
	assert(nmls.size() == tgts.size());
	assert(nmls.size() % 3 == 0);

	std::vector<GLfloat> dest(nmls.size());

	unsigned k = 0;

	while(k != dest.size())
	{
		T nx = nmls[k+0];
		T ny = nmls[k+1];
		T nz = nmls[k+2];

		T tx = tgts[k+0];
		T ty = tgts[k+1];
		T tz = tgts[k+2];

		dest[k++] = T(ny*tz-nz*ty);
		dest[k++] = T(nz*tx-nx*tz);
		dest[k++] = T(nx*ty-ny*tx);
	}

	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> WickerTorus::_tex_coords(void) const
{
	typedef GLfloat T;
	std::vector<GLfloat> dest(
		2*2*2*_rings*(_sections*3 + 1)*2+
		2*2*2*_sections*(_rings*2 + 1)*2
	);
	unsigned k = 0;
	//
	GLdouble t = _thickness / _radius_in;
	GLdouble r_step = 0.5 / GLdouble(_rings);
	GLdouble s_step = 1.0 / GLdouble(_sections);
	GLdouble r_slip = r_step * _r_slip_coef;
	GLdouble s_slip = s_step * t;

	for(unsigned f=0; f!=2; ++f)
	{
		for(unsigned r=0; r!=_rings; ++r)
		{
			GLdouble rv = 2*r*r_step;
			for(unsigned s=0; s!=_sections; ++s)
			{
				const GLdouble sa[3] = {
					s*s_step,
					(s + t)*s_step,
					(s + 1.0 - 2*t)*s_step
				};
				for(unsigned p=0; p!=3; ++p)
				{
					GLdouble v = sa[p];
					for(unsigned d=0; d!=2; ++d)
					{
						GLdouble u = rv + ((d+f)%2)*r_step;
						dest[k++] = T(u);
						dest[k++] = T(v);
					}
				}
			}
			for(unsigned d=0; d!=2; ++d)
			{
				GLdouble u = rv + ((d+f)%2)*r_step;
				dest[k++] = T(u);
				dest[k++] = T(1.0);
			}
		}
	}

	for(unsigned d=0; d!=2; ++d)
	{
		for(unsigned r=0; r!=_rings; ++r)
		{
			GLdouble rv = 2*r*r_step;
			for(unsigned s=0; s!=_sections; ++s)
			{
				const GLdouble sa[3] = {
					s*s_step,
					(s + t)*s_step,
					(s + 1.0 - 2*t)*s_step
				};
				for(unsigned p=0; p!=3; ++p)
				{
					GLdouble v = sa[p];
					for(unsigned f=0; f!=2; ++f)
					{
						GLdouble u = rv + ((d+f)%2)*r_step;
						dest[k++] = T(u);
						dest[k++] = T(v);
					}
				}
			}
			for(unsigned f=0; f!=2; ++f)
			{
				GLdouble u = rv + ((d+f)%2)*r_step;
				dest[k++] = T(u);
				dest[k++] = T(1.0);
			}
		}
	}

	s_slip = s_step * _s_slip_coef;

	for(unsigned f=0; f!=2; ++f)
	{
		const GLdouble f_sign = (f == 0)? 1.0: -1.0;
		for(unsigned s=0; s!=_sections; ++s)
		{
			const GLdouble s_angle = s_step*0.5 + s*s_step;
			const GLdouble sa[2] = {
				s_angle + s_slip*f_sign,
				s_angle - s_slip*f_sign
			};
			for(unsigned r=0; r!=_rings; ++r)
			{
				const GLdouble r_angle = 2*r*r_step;
				const GLdouble ra[2] = {
					r_angle + r_slip,
					r_angle + r_step - r_slip
				};
				for(unsigned p=0; p!=2; ++p)
				{
					GLdouble u = ra[p];
					for(unsigned d=0; d!=2; ++d)
					{
						GLdouble v = sa[d];
						dest[k++] = T(u);
						dest[k++] = T(v);
					}
				}
			}

			GLdouble u = 1.0 + r_slip;
			for(unsigned d=0; d!=2; ++d)
			{
				GLdouble v = sa[d];
				dest[k++] = T(u);
				dest[k++] = T(v);
			}
		}
	}

	for(unsigned d=0; d!=2; ++d)
	{
		const GLdouble d_sign = (d == 0)? 1.0: -1.0;
		for(unsigned s=0; s!=_sections; ++s)
		{
			const GLdouble s_angle = s_step*0.5 + s*s_step;
			const GLdouble v = s_angle + s_slip*d_sign;
			for(unsigned r=0; r!=_rings; ++r)
			{
				const GLdouble r_angle = 2*r*r_step;
				const GLdouble ra[2] = {
					r_angle + r_slip,
					r_angle + r_step - r_slip
				};
				for(unsigned p=0; p!=2; ++p)
				{
					GLdouble u = ra[p];
					for(unsigned f=0; f!=2; ++f)
					{
						dest[k++] = T(u);
						dest[k++] = T(v);
					}
				}
			}

			GLdouble u = 1.0 + r_slip;
			for(unsigned f=0; f!=2; ++f)
			{
				dest[k++] = T(u);
				dest[k++] = T(v);
			}
		}
	}

	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
DrawingInstructions
WickerTorus::Instructions(WickerTorus::Default) const
{
	auto instructions = this->MakeInstructions();
	unsigned strip = 2*(_sections*3 + 1);
	unsigned offs = 0;

	GLuint phase = 0;
	for(unsigned f=0; f!=4; ++f)
	{
		for(unsigned r=0; r!=_rings; ++r)
		{
			DrawOperation operation;
			operation.method = DrawOperation::Method::DrawArrays;
			operation.mode = PrimitiveType::TriangleStrip;
			operation.first = GLuint(offs);
			operation.count = GLuint(strip);
			operation.restart_index = DrawOperation::NoRestartIndex();
			operation.phase = phase;
			this->AddInstruction(instructions, operation);
			offs += strip;
		}
		++phase;
	}

	strip = 2*(_rings*2 + 1);

	for(unsigned f=0; f!=4; ++f)
	{
		for(unsigned s=0; s!=_sections; ++s)
		{
			DrawOperation operation;
			operation.method = DrawOperation::Method::DrawArrays;
			operation.mode = PrimitiveType::TriangleStrip;
			operation.first = GLuint(offs);
			operation.count = GLuint(strip);
			operation.restart_index = DrawOperation::NoRestartIndex();
			operation.phase = phase;
			this->AddInstruction(instructions, operation);
			offs += strip;
		}
		++phase;
	}
	return instructions;
}

OGLPLUS_LIB_FUNC
WickerTorus::IndexArray
WickerTorus::Indices(WickerTorus::Edges) const
{
	const unsigned leap_1 = 2*_rings*(_sections*3 + 1);
	const unsigned leap_2 = 2*_sections*(_rings*2 + 1);
	IndexArray indices(2*leap_1+2*leap_2);
	unsigned k = 0;

	for(unsigned f=0; f!=2; ++f)
	{
		for(unsigned d=0; d!=2; ++d)
		{
			unsigned i = f*leap_1 + d;
			for(unsigned r=0; r!=_rings; ++r)
			{
				for(unsigned s=0; s!=_sections; ++s)
				{
					for(unsigned p=0; p!=3; ++p)
						indices[k++] = i += 2;
				}
				indices[k++] = i += 2;
			}
		}
	}

	for(unsigned f=0; f!=2; ++f)
	{
		for(unsigned d=0; d!=2; ++d)
		{
			unsigned i = 4*leap_1 + f*leap_2 + d;
			for(unsigned s=0; s!=_sections; ++s)
			{
				for(unsigned r=0; r!=_rings; ++r)
				{
					for(unsigned p=0; p!=2; ++p)
						indices[k++] = i += 2;
				}
				indices[k++] = i += 2;
			}
		}
	}

	assert(k == indices.size());

	return indices;
}

OGLPLUS_LIB_FUNC
DrawingInstructions
WickerTorus::Instructions(WickerTorus::Edges) const
{
	auto instructions = this->MakeInstructions();

	GLuint phase = 0;
	unsigned edge = (_sections*3 + 1);
	unsigned offs = 0;

	for(unsigned f=0; f!=2; ++f)
	{
		for(unsigned d=0; d!=2; ++d)
		{
			for(unsigned r=0; r!=_rings; ++r)
			{
				DrawOperation operation;
				operation.method = DrawOperation::Method::DrawElements;
				operation.mode = PrimitiveType::LineLoop;
				operation.first = GLuint(offs);
				operation.count = GLuint(edge-1);
				operation.restart_index = DrawOperation::NoRestartIndex();
				operation.phase = phase;
				this->AddInstruction(instructions, operation);
				offs += edge;
			}
		}
	}

	edge = (_rings*2 + 1);

	for(unsigned f=0; f!=2; ++f)
	{
		for(unsigned d=0; d!=2; ++d)
		{
			for(unsigned s=0; s!=_sections; ++s)
			{
				DrawOperation operation;
				operation.method = DrawOperation::Method::DrawElements;
				operation.mode = PrimitiveType::LineLoop;
				operation.first = GLuint(offs);
				operation.count = GLuint(edge-1);
				operation.restart_index = DrawOperation::NoRestartIndex();
				operation.phase = phase;
				this->AddInstruction(instructions, operation);
				offs += edge;
			}
		}
	}

	offs = 0;

	for(unsigned f=0; f!=2; ++f)
	{
		for(unsigned r=0; r!=_rings; ++r)
		{
			DrawOperation operation;
			operation.method = DrawOperation::Method::DrawArrays;
			operation.mode = PrimitiveType::Lines;
			operation.first = GLuint(offs);
			operation.count = GLuint(2*edge);
			operation.restart_index = DrawOperation::NoRestartIndex();
			operation.phase = phase;
			this->AddInstruction(instructions, operation);
			offs += 2*edge;
		}
	}

	return instructions;
}

} // shapes
} // oglplus

