%undefine _hardened_build
%define _gprdir %_GNAT_project_dir
%define rtl_version 0.1

Name:       increment
Version:    0.1.0
Release:    git%{?dist}
Summary:    Incremental analysis library
Group:      Development/Libraries
License:    MIT
URL:        https://github.com/reznikmm/increment
### Direct download is not availeble
Source0:    increment.tar.gz
BuildRequires:   gcc-gnat
BuildRequires:   fedora-gnat-project-common  >= 3 
BuildRequires:   matreshka-devel
BuildRequires:   gprbuild

# gprbuild only available on these:
ExclusiveArch: %GPRbuild_arches

%description
Incremental analysis in Ada (increment)

This package provides incremental analysis algorithms and related data structures. The main target of the project is construction of integrated development environment (IDE).

The library perfoms a lexical and syntactical analisys of a program text and construct a parsing tree. As text changes are introduced in the tree and subsequent analisys pass restores consistent parsing tree for new text. Unaffected parts of the tree are kept unchanged.

We try to (re-)implement ideas described by Tim A. Wagner his work "Practical Algorithms for Incremental Software Development Environments"

%package devel

Group:      Development/Libraries
License:    MIT
Summary:    Devel package for Increment
Requires:       %{name}%{?_isa} = %{version}-%{release}
Requires:   fedora-gnat-project-common  >= 2

%description devel
Devel package for Increment

%prep 
%setup -q -n %{name}

%build
make  %{?_smp_mflags} GPRBUILD_FLAGS="%Gnatmake_optflags"

%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot} LIBDIR=%{_libdir} PREFIX=%{_prefix} GPRDIR=%{_gprdir} BINDIR=%{_bindir}

%check
make  %{?_smp_mflags} GPRBUILD_FLAGS="%Gnatmake_optflags" check

%post     -p /sbin/ldconfig
%postun   -p /sbin/ldconfig

%files
%doc LICENSE
%dir %{_libdir}/%{name}
%{_libdir}/%{name}/libincr.so.%{rtl_version}
%{_libdir}/libincr.so.%{rtl_version}
%{_libdir}/%{name}/libincr.so.0
%{_libdir}/libincr.so.0

%files devel
%doc README.md
%{_libdir}/%{name}/libincr.so
%{_libdir}/libincr.so
%{_libdir}/%{name}/*.ali
%{_includedir}/%{name}
%{_gprdir}/increment.gpr
%{_gprdir}/manifests/increment


%changelog
* Sun Nov 12 2017 Maxim Reznik <reznikmm@gmail.com> - 0.1.0-git
- Initial package
