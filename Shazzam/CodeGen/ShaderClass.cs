using Microsoft.CodeDom.Providers.DotNetCompilerPlatform;
using System.Windows.Media.Media3D;
using System.Collections.Generic;
using System.CodeDom.Compiler;
using System.Windows.Media;
using System.Reflection;
using System.Windows;
using System.CodeDom;
using System.Text;
using System.Linq;
using System.IO;
using System;

using Shazzam.Converters;
using Shazzam.Properties;
using System.Windows.Media.Effects;

namespace Shazzam.CodeGen
{
    internal static class ShaderClass
    {
        public static string GetSourceText(CodeDomProvider currentProvider, ShaderModel model, bool includePixelShaderConstructor)
        {
            if (currentProvider is CSharpCodeProvider _)
            {
                List<(string name, string type)> props = new List<(string name, string type)>();

                string generate_dependency_props()
                {
                    StringBuilder sb = new StringBuilder();

                    foreach (ShaderModelConstantRegister register in model.Registers.Concat(new ShaderModelConstantRegister[] {
                        new ShaderModelConstantRegister("Input", typeof(Brush), 0, null, null, null, null),
                    }))
                    {
                        props.Add((register.RegisterName, register.RegisterType.Name));

                        sb.Append($"        public static readonly DependencyProperty {register.RegisterName}Property = ");

                        if (typeof(Brush).IsAssignableFrom(register.RegisterType))
                            sb.AppendLine($"RegisterPixelShaderSamplerProperty(nameof({register.RegisterName}), typeof(§classname§), {register.RegisterNumber});");
                        else
                            sb.AppendLine($"Register(nameof({register.RegisterName}), typeof({register.RegisterType}), typeof(§classname§), default({register.RegisterType}), {register.RegisterNumber});");
                    }

                    return sb.ToString();
                }
                string generate_props() => string.Join("\n", props.Select(p => $@"
        public {p.type} {p.name}
        {{
            get => ({p.type})GetValue({p.name}Property);
            set => SetValue({p.name}Property, value);
        }}"));
                string generate_update_instr() => string.Join("\n            ", props.Select(p => $"UpdateShaderValue({p.name}Property);"));

                Dictionary<string, object> vars = new Dictionary<string, object>
                {
                    ["namespace"] = model.GeneratedNamespace,
                    ["classname"] = model.GeneratedClassName,
                    ["depprops"] = generate_dependency_props(),
                    ["props"] = generate_props(),
                    ["updateinstr"] = generate_update_instr(),
                    ["shaderctor"] = includePixelShaderConstructor ? "        public §classname§(PixelShader shader) : base(shader) { }" : "",
                };

                return @"
using System.Windows.Media.Media3D;
using System.Windows.Media.Effects;
using System.Windows.Media;
using System.Windows;
using System;


namespace §namespace§
{
    public abstract class PixelShaderEffectBase
        : ShaderEffect
    {
        private static readonly string asmname = typeof(PixelShaderEffectBase).Assembly.ToString().Split(',')[0];

        private static Uri GetUri(string name) => new Uri($""pack://application:,,,/{asmname};component/{name}"");

        private static string TrimEnd(string input, string suffix, StringComparison cmp = StringComparison.InvariantCulture) =>
            (suffix != null) && (input?.EndsWith(suffix, cmp) ?? false) ? input.Substring(0, input.Length - suffix.Length) : input;

        protected static DependencyProperty Register(string name, Type type, Type parent, object @default, int slot) =>
            DependencyProperty.Register(name, type, parent, new UIPropertyMetadata(@default, PixelShaderConstantCallback(slot)));


        internal PixelShaderEffectBase(string name)
            : this(new PixelShader
            {
                UriSource = GetUri(name)
            })
        {
        }

        internal PixelShaderEffectBase(PixelShader shader)
        {
            PixelShader = shader;

            UpdateShader();
        }

        internal protected abstract void UpdateShader();
    }


    public sealed class §classname§
        : PixelShaderEffectBase
    {
§depprops§
§props§
§shaderctor§

        public §classname§()
            : base(""§classname§.ps"")
        {
        }
    
        protected internal override void UpdateShader()
        {
            §updateinstr§
        }
    }
}".DFormat(vars).DFormat(vars).Trim().Replace("System.Double", "double").Replace("(Double)", "(double)"); // so what? Sue me!
            }
            else if (currentProvider is Microsoft.CSharp.CSharpCodeProvider csprv)
                return ___old__ShaderClass.GetSourceText(csprv, model, includePixelShaderConstructor);
            else
                throw new Exception($"No code gen support currently exists for `{currentProvider?.GetType()?.FullName}`.");
        }

        internal static string DFormat(this string formatstring, Dictionary<string, object> dic)
        {
            Dictionary<string, int> kti = new Dictionary<string, int>();
            StringBuilder sb = new StringBuilder(formatstring);
            int i = 0;

            sb = sb.Replace("{", "{{")
                   .Replace("}", "}}");

            foreach (var tuple in dic)
            {
                sb = sb.Replace($"§{tuple.Key}§", $"{{{i}}}");

                kti.Add(tuple.Key, i);

                ++i;
            }
            
            sb = sb.Replace("§§", "§");

            return string.Format(sb.ToString(), dic.OrderBy(x => kti[x.Key]).Select(x => x.Value).ToArray());
        }

        public static Assembly CompileInMemory(string code)
        {
            using (CSharpCodeProvider provider = new CSharpCodeProvider())
            {
                CompilerResults compiled = provider.CompileAssemblyFromSource(new CompilerParameters
                {
                    ReferencedAssemblies = {
                        typeof(Uri).Assembly.Location, // System.dll
                        typeof(HashSet<>).Assembly.Location, // System.Core.dll
                        typeof(DependencyProperty).Assembly.Location, // WindowsBase.dll
                        typeof(Application).Assembly.Location, // PresentationFramework.dll
                        typeof(PixelShader).Assembly.Location, // PresentationCore.dll
                    },
                    IncludeDebugInformation = false,
                    GenerateExecutable = false,
                    GenerateInMemory = true
                }, code);

                if (compiled.Errors.Count == 0)
                    return compiled.CompiledAssembly;
                else
                    throw new InvalidOperationException(string.Join(Environment.NewLine, compiled.Errors.OfType<CompilerError>().Select(e => e.ErrorText)));
            }
        }
    }

    [Obsolete("Use '" + nameof(ShaderClass) + "' instead.")]
    internal static class ___old__ShaderClass
    {
        public static string GetSourceText(CodeDomProvider currentProvider, ShaderModel shaderModel, bool includePixelShaderConstructor)
        {
            return GenerateCode(
                currentProvider,
                BuildPixelShaderGraph(shaderModel, includePixelShaderConstructor),
                shaderModel);
        }

        public static Assembly CompileInMemory(string code)
        {
            using (var provider = new Microsoft.CSharp.CSharpCodeProvider(new Dictionary<string, string> { { "CompilerVersion", "v3.5" } }))
            {
                var options = new CompilerParameters
                {
                    ReferencedAssemblies =
                    {
                        "System.dll",
                        "System.Core.dll",
                        "WindowsBase.dll",
                        "PresentationFramework.dll",
                        "PresentationCore.dll",
                    },
                    IncludeDebugInformation = false,
                    GenerateExecutable = false,
                    GenerateInMemory = true
                };
                var compiled = provider.CompileAssemblyFromSource(options, code);
                if (compiled.Errors.Count == 0)
                {
                    return compiled.CompiledAssembly;
                }

                throw new InvalidOperationException(string.Join(Environment.NewLine, compiled.Errors.OfType<CompilerError>().Select(e => e.ErrorText)));
            }
        }

        private static CodeCompileUnit BuildPixelShaderGraph(ShaderModel shaderModel, bool includePixelShaderConstructor)
        {
            // Create a new CodeCompileUnit to contain
            // the program graph.
            var codeGraph = new CodeCompileUnit();

            // Create the namespace.
            var codeNamespace = AssignNamespacesToGraph(codeGraph, shaderModel.GeneratedNamespace);

            // Create the appropriate constructor.
            var constructor = includePixelShaderConstructor ? CreatePixelShaderConstructor(shaderModel) : CreateDefaultConstructor(shaderModel);

            // Declare a new type.
            var shader = new CodeTypeDeclaration
            {
                Name = shaderModel.GeneratedClassName,
                BaseTypes =
                {
                    new CodeTypeReference("ShaderEffect")
                },
                Members =
                {
                    constructor,
                    CreateSamplerDependencyProperty(shaderModel.GeneratedClassName, "Input"),
                    CreateClrProperty("Input", typeof(Brush), null)
                },
            };
            if (!string.IsNullOrEmpty(shaderModel.Description))
            {
                shader.Comments.Add(new CodeCommentStatement($"<summary>{shaderModel.Description}</summary>", docComment: true));
            }

            // Add a dependency property and a CLR property for each of the shader's register variables.
            foreach (var register in shaderModel.Registers)
            {
                shader.Members.Add(CreateShaderRegisterDependencyProperty(shaderModel, register));
                shader.Members.Add(CreateClrProperty(register.RegisterName, register.RegisterType, register.Description));
            }

            if (!includePixelShaderConstructor)
            {
                shader.Members.Add(CreateShaderField(shaderModel));
            }

            // Add the new type to the namespace.
            codeNamespace.Types.Add(shader);

            return codeGraph;
        }

        private static CodeMemberField CreateShaderField(ShaderModel model)
        {
            var typeReference = new CodeTypeReference("PixelShader");
            return new CodeMemberField
            {
                Type = typeReference,
                Name = "Shader",
                //// ReSharper disable once BitwiseOperatorOnEnumWithoutFlags
                Attributes = MemberAttributes.Static | MemberAttributes.Private,
                InitExpression = new CodeObjectCreateExpression { CreateType = typeReference },
                Comments =
                {
                    new CodeCommentStatement("<summary>", docComment: true),
                    new CodeCommentStatement($"The uri should be something like pack://application:,,,/Gu.Wpf.Geometry;component/Effects/{model.GeneratedClassName}.ps", docComment: true),
                    new CodeCommentStatement($"The file {model.GeneratedClassName}.ps should have BuildAction: Resource", docComment: true),
                    new CodeCommentStatement("</summary>", docComment: true)
                }
            };
        }

        private static CodeMemberField CreateSamplerDependencyProperty(string className, string propertyName)
        {
            return new CodeMemberField
            {
                Type = new CodeTypeReference("DependencyProperty"),
                Name = $"{propertyName}Property",
                //// ReSharper disable once BitwiseOperatorOnEnumWithoutFlags
                Attributes = MemberAttributes.Static | MemberAttributes.Public,
                InitExpression = new CodeMethodInvokeExpression
                {
                    Method = new CodeMethodReferenceExpression
                    {
                        TargetObject = new CodeTypeReferenceExpression("ShaderEffect"),
                        MethodName = "RegisterPixelShaderSamplerProperty"
                    },
                    Parameters =
                    {
                        new CodePrimitiveExpression(propertyName),
                        new CodeTypeOfExpression(className),
                        new CodePrimitiveExpression(0)
                    }
                }
            };
        }

        private static CodeMemberField CreateShaderRegisterDependencyProperty(ShaderModel shaderModel, ShaderModelConstantRegister register)
        {
            if (typeof(Brush).IsAssignableFrom(register.RegisterType))
            {
                return new CodeMemberField
                {
                    Comments =
                        {
                            new CodeCommentStatement("<summary>The ShaderEffect.RegisterPixelShaderSamplerProperty() method must be used with this field as argument. Note the last parameter of this method: it is an integer and it corresponds to the S0 pixel shader register.</summary>"),
                        },
                    Type = new CodeTypeReference("DependencyProperty"),
                    Name = $"{register.RegisterName}Property",
                    //// ReSharper disable once BitwiseOperatorOnEnumWithoutFlags
                    Attributes = MemberAttributes.Public | MemberAttributes.Static,
                    InitExpression = new CodeMethodInvokeExpression
                    {
                        Method = new CodeMethodReferenceExpression
                        {
                            TargetObject = new CodeTypeReferenceExpression("ShaderEffect"),
                            MethodName = "RegisterPixelShaderSamplerProperty"
                        },
                        Parameters =
                        {
                            new CodePrimitiveExpression(register.RegisterName),
                            new CodeTypeOfExpression(shaderModel.GeneratedClassName),
                            new CodePrimitiveExpression(register.RegisterNumber)
                        }
                    },
                };
            }

            return new CodeMemberField
            {
                Type = new CodeTypeReference("DependencyProperty"),
                Name = $"{register.RegisterName}Property",
                //// ReSharper disable once BitwiseOperatorOnEnumWithoutFlags
                Attributes = MemberAttributes.Public | MemberAttributes.Static,
                InitExpression = new CodeMethodInvokeExpression
                {
                    Method = new CodeMethodReferenceExpression
                    {
                        TargetObject = new CodeTypeReferenceExpression("DependencyProperty"),
                        MethodName = "Register"
                    },
                    Parameters =
                        {
                            new CodePrimitiveExpression(register.RegisterName),
                            new CodeTypeOfExpression(CreateCodeTypeReference(register.RegisterType)),
                            new CodeTypeOfExpression(shaderModel.GeneratedClassName),
                            new CodeObjectCreateExpression
                            {
                                // Silverlight doesn't have UIPropertyMetadata.
                                CreateType = new CodeTypeReference(shaderModel.TargetFramework == TargetFramework.WPF ? "UIPropertyMetadata" : "PropertyMetadata"),
                                Parameters =
                                {
                                CreateDefaultValue(register.RegisterType, register.DefaultValue),
                                new CodeMethodInvokeExpression
                                    {
                                        Method = new CodeMethodReferenceExpression(null, "PixelShaderConstantCallback"),
                                        Parameters =
                                            {
                                                new CodePrimitiveExpression(register.RegisterNumber)
                                            }
                                    }
                            }
                        }
                    }
                }
            };
        }

        private static CodeExpression CreateDefaultValue(Type type, object defaultValue)
        {
            if (defaultValue == null)
            {
                return new CodePrimitiveExpression(null);
            }

            var codeTypeReference = CreateCodeTypeReference(type);
            if (defaultValue.GetType().IsPrimitive)
            {
                return new CodePrimitiveExpression(Convert.ChangeType(defaultValue, type));
            }

            if (defaultValue is Point || defaultValue is Vector || defaultValue is Size)
            {
                var point = (Point)RegisterValueConverter.ConvertToUsualType(defaultValue);
                return new CodeObjectCreateExpression(
                    codeTypeReference,
                    new CodePrimitiveExpression(point.X),
                    new CodePrimitiveExpression(point.Y));
            }

            if (defaultValue is Point3D || defaultValue is Vector3D)
            {
                var point3D = (Point3D)RegisterValueConverter.ConvertToUsualType(defaultValue);
                return new CodeObjectCreateExpression(
                    codeTypeReference,
                    new CodePrimitiveExpression(point3D.X),
                    new CodePrimitiveExpression(point3D.Y),
                    new CodePrimitiveExpression(point3D.Z));
            }

            if (defaultValue is Point4D point4D)
            {
                return new CodeObjectCreateExpression(
                    codeTypeReference,
                    new CodePrimitiveExpression(point4D.X),
                    new CodePrimitiveExpression(point4D.Y),
                    new CodePrimitiveExpression(point4D.Z),
                    new CodePrimitiveExpression(point4D.W));
            }

            if (defaultValue is Color color)
            {
                return new CodeMethodInvokeExpression(
                    new CodeTypeReferenceExpression(codeTypeReference),
                    "FromArgb",
                    new CodePrimitiveExpression(color.A),
                    new CodePrimitiveExpression(color.R),
                    new CodePrimitiveExpression(color.G),
                    new CodePrimitiveExpression(color.B));
            }

            return new CodeDefaultValueExpression(codeTypeReference);
        }

        private static CodeMemberProperty CreateClrProperty(string propertyName, Type type, string description)
        {
            var property = new CodeMemberProperty
            {
                Name = propertyName,
                Type = CreateCodeTypeReference(type),
                //// ReSharper disable once BitwiseOperatorOnEnumWithoutFlags
                Attributes = MemberAttributes.Public | MemberAttributes.Final,
                HasGet = true,
                GetStatements =
                {
                    new CodeMethodReturnStatement
                    {
                        Expression = new CodeCastExpression
                        {
                            TargetType = CreateCodeTypeReference(type),
                            Expression = new CodeMethodInvokeExpression
                            {
                                Method = new CodeMethodReferenceExpression(new CodeThisReferenceExpression(), "GetValue"),
                                Parameters = { new CodeVariableReferenceExpression($"{propertyName}Property") }
                            }
                        }
                    }
                },
                HasSet = true,
                SetStatements =
                {
                    new CodeMethodInvokeExpression
                    {
                        Method = new CodeMethodReferenceExpression(new CodeThisReferenceExpression(), "SetValue"),
                        Parameters =
                        {
                            new CodeVariableReferenceExpression(propertyName + "Property"),
                            new CodeVariableReferenceExpression("value")
                        }
                    }
                }
            };

            if (type == typeof(Brush))
            {
                property.Comments.Add(new CodeCommentStatement("<summary>", docComment: true));
                property.Comments.Add(new CodeCommentStatement(
                    $"There has to be a property of type Brush called \"Input\". This property contains the input image and it is usually not set directly - it is set automatically when our effect is applied to a control.",
                    docComment: true));
                property.Comments.Add(new CodeCommentStatement("</summary>", docComment: true));
            }

            if (!string.IsNullOrEmpty(description))
            {
                property.Comments.Add(new CodeCommentStatement($"<summary>{description}</summary>", docComment: true));
            }

            return property;
        }

        private static CodeTypeReference CreateCodeTypeReference(Type type)
        {
            return type.IsPrimitive ? new CodeTypeReference(type) : new CodeTypeReference(type.Name);
        }

        private static CodeConstructor CreatePixelShaderConstructor(ShaderModel shaderModel)
        {
            // Create a constructor that takes a PixelShader as its only parameter.
            var constructor = new CodeConstructor
            {
                Attributes = MemberAttributes.Public,
                Parameters =
                {
                    new CodeParameterDeclarationExpression("PixelShader", "shader")
                },
                Statements =
                {
                    new CodeAssignStatement
                    {
                        Left = new CodePropertyReferenceExpression(new CodeThisReferenceExpression(), "PixelShader"),
                        Right = new CodeArgumentReferenceExpression("shader")
                    },
                    CreateUpdateMethod("Input")
                }
            };
            foreach (var register in shaderModel.Registers)
            {
                constructor.Statements.Add(CreateUpdateMethod(register.RegisterName));
            }

            return constructor;
        }

        private static CodeConstructor CreateDefaultConstructor(ShaderModel shaderModel)
        {
            var constructor = new CodeConstructor
            {
                Attributes = MemberAttributes.Public,
                Statements =
                {
                    new CodeAssignStatement
                    {
                        Left = new CodePropertyReferenceExpression(new CodeThisReferenceExpression(), "PixelShader"),
                        Right = new CodeFieldReferenceExpression(null, "Shader")
                    },
                    CreateUpdateMethod("Input")
                }
            };
            foreach (var register in shaderModel.Registers)
            {
                constructor.Statements.Add(CreateUpdateMethod(register.RegisterName));
            }

            return constructor;
        }

        private static CodeMethodInvokeExpression CreateUpdateMethod(string propertyName)
        {
            return new CodeMethodInvokeExpression
            {
                Method = new CodeMethodReferenceExpression(new CodeThisReferenceExpression(), "UpdateShaderValue"),
                Parameters =
                {
                    new CodeVariableReferenceExpression(propertyName + "Property")
                }
            };
        }

        private static CodeNamespace AssignNamespacesToGraph(CodeCompileUnit codeGraph, string namespaceName)
        {
            var ns = new CodeNamespace(namespaceName);
            codeGraph.Namespaces.Add(ns);
            codeGraph.Namespaces.Add(
                new CodeNamespace
                {
                    Imports =
                            {
                                new CodeNamespaceImport("System"),
                                new CodeNamespaceImport("System.Windows"),
                                new CodeNamespaceImport("System.Windows.Media"),
                                new CodeNamespaceImport("System.Windows.Media.Effects"),
                                new CodeNamespaceImport("System.Windows.Media.Media3D")
                            }
                });

            return ns;
        }

        private static string GenerateCode(CodeDomProvider provider, CodeCompileUnit compileUnit, ShaderModel model)
        {
            // Generate source code using the code generator.
            using (var writer = new StringWriter())
            {
                var indentString = Settings.Default.IndentUsingTabs
                                       ? "\t"
                                       : new string(' ', Settings.Default.IndentSpaces);
                var options = new CodeGeneratorOptions
                {
                    IndentString = indentString,
                    BlankLinesBetweenMembers = true,
                    BracingStyle = "C",
                };
                provider.GenerateCodeFromCompileUnit(compileUnit, writer, options);
                var text = writer.ToString();
                //// Fix up code: make static DP fields readonly, and use triple-slash or triple-quote comments for XML doc comments.
                if (provider.FileExtension == "cs")
                {
                    text = text.Replace(
                                   "private static PixelShader Shader = new PixelShader()",
                                   $"private static readonly PixelShader Shader = new PixelShader{Environment.NewLine}" +
                                   $"        {{{Environment.NewLine}" +
                                   $"            UriSource = new Uri(\"pack://application:,,,/[assemblyname];component/[folder]/{model.GeneratedClassName}.ps\", UriKind.Absolute){Environment.NewLine}" +
                                   $"        }}")
                               .Replace(
                                   "public static DependencyProperty",
                                   "public static readonly DependencyProperty")
                               .Replace($"{writer.NewLine}    {writer.NewLine}", $"{writer.NewLine}{writer.NewLine}")
                               .Replace($"{writer.NewLine}        {writer.NewLine}", $"{writer.NewLine}{writer.NewLine}")
                               .Replace($"{writer.NewLine}{writer.NewLine}{writer.NewLine}", $"{writer.NewLine}{writer.NewLine}")
                               .Replace($"{{{writer.NewLine}{writer.NewLine}", $"{{{writer.NewLine}");
                }
                else if (provider.FileExtension == "vb")
                {
                    text = text.Replace("Public Shared ", "Public Shared ReadOnly ");
                    text = text.Replace("'<", "'''<");
                }

                return text;
            }
        }
    }
}
